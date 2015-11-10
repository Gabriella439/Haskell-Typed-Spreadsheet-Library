{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RankNTypes                 #-}

-- | The following program:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Typed.Spreadsheet
-- > 
-- > main :: IO ()
-- > main = textUI "Example program" logic
-- >   where
-- >     logic = combine <$> checkBox   "a"
-- >                     <*> spinButton "b" 1
-- >                     <*> spinButton "c" 0.1
-- >                     <*> entry      "d"
-- > 
-- >     combine a b c d = display (a, b + c, d)
--
-- ... creates a user interface that looks like this:
--
-- <<http://i.imgur.com/xPifEtA.png User interface on startup>>
--
-- Every time you update a control on the left panel, the right panel updates
-- in response:
--
-- <<http://i.imgur.com/TTxgSwN.png User interface after user input>>
--
-- Once @ghc-8.0@ is out then you can simplify the above program even further
-- using the `ApplicativeDo` extension:
--
-- > {-# LANGUAGE ApplicativeDo     #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Typed.Spreadsheet
-- > 
-- > main :: IO ()
-- > main = textUI "Example program" (do
-- >     a <- checkBox   "a"
-- >     b <- spinButton "b" 1
-- >     c <- spinButton "c" 0.1
-- >     d <- entry      "d"
-- >     return (display (a, b + c, d)) )
--
-- The general workflow for this library is:
--
-- * You build primitive `Updatable` values using `checkBox`, `spinButton`,
--   `entry`, or `radioButton`, each of which corresponds to a control on the
--   left panel of the user interface
-- * You transform or combine `Updatable` values using `Functor` and
--   `Applicative` operations.  Composite values update whenever one of their
--   substituent values update
-- * You consume an @(`Updatable` `Text`)@ value using `textUI`, which displays
--   the continuously updating value in the right panel of the user interface
--
-- You can get started quickly by cloning and building this project:
--
-- > $ git clone https://github.com/Gabriel439/Haskell-Typed-Spreadsheet-Library.git
-- > $ stack build --install-ghc             # Builds the executable
-- > $ stack exec typed-spreadsheet-example  # Runs the executable
--
-- That project includes the code for the above example in @exec/Main.hs@.  Just
-- modify that file and rebuild to play with the example.
--
-- NOTE: You must compile your program with the @-threaded@ flag.  The example
-- project takes care of this.
--
-- See the \"Examples\" section at the bottom of this module for more examples.

module Typed.Spreadsheet (
    -- * Types
      Updatable
    , textUI

    -- * Controls
    , checkBox
    , spinButton
    , entry
    , radioButton

    -- * Utilities
    , display

    -- * Examples
    -- $examples
    ) where

import Control.Applicative
import Control.Concurrent.STM (STM)
import Control.Foldl (Fold(..))
import Control.Monad.IO.Class (liftIO)
import Data.Monoid
import Data.String (IsString(..))
import Data.Text (Text)
import Lens.Micro (_Left, _Right)
import Graphics.UI.Gtk (AttrOp((:=)))

import qualified Control.Concurrent.STM   as STM
import qualified Control.Concurrent.Async as Async
import qualified Control.Foldl            as Fold
import qualified Data.Text                as Text
import qualified Graphics.UI.Gtk          as Gtk

data Cell a = forall e . Cell (IO (STM e, Fold e a))

instance Functor Cell where
    fmap f (Cell m) = Cell (fmap (fmap (fmap f)) m)

instance Applicative Cell where
    pure a = Cell (pure (empty, pure a))

    Cell mF <*> Cell mX = Cell (liftA2 helper mF mX)
      where
        helper (inputF, foldF) (inputX, foldX) = (input, fold )
          where
            input = fmap Left inputF <|> fmap Right inputX

            fold = Fold.handles _Left foldF <*> Fold.handles _Right foldX

-- | An updatable input value
data Updatable a = Updatable (Control -> Cell a)

instance Functor Updatable where
    fmap f (Updatable m) = Updatable (fmap (fmap f) m)

instance Applicative Updatable where
    pure a = Updatable (pure (pure a))

    Updatable mf <*> Updatable mx = Updatable (liftA2 (<*>) mf mx)

instance Monoid a => Monoid (Updatable a) where
    mempty = pure mempty

    mappend = liftA2 mappend

instance IsString a => IsString (Updatable a) where
    fromString str = pure (fromString str)

instance Num a => Num (Updatable a) where
    fromInteger = pure . fromInteger

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance Fractional a => Fractional (Updatable a) where
    fromRational = pure . fromRational

    recip = fmap recip

    (/) = liftA2 (/)

instance Floating a => Floating (Updatable a) where
    pi = pure pi

    exp   = fmap exp
    sqrt  = fmap sqrt
    log   = fmap log
    sin   = fmap sin
    tan   = fmap tan
    cos   = fmap cos
    asin  = fmap sin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    tanh  = fmap tanh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acosh

    (**)    = liftA2 (**)
    logBase = liftA2 logBase

-- | Use a `Control` to obtain updatable input `Updatable`s
data Control = Control
    { _checkBox    :: Text -> Cell Bool
    , _spinButton  :: Text -> Double -> Cell Double
    , _entry       :: Text -> Cell Text
    , _radioButton :: forall a . Show a => Text -> a -> [a] -> Cell a
    }

-- | Build a `Text`-based user interface
textUI
    :: Text
    -- ^ Window title
    -> Updatable Text
    -- ^ Program logic
    -> IO ()
textUI title (Updatable k) = do
    _ <- Gtk.initGUI

    window <- Gtk.windowNew
    Gtk.set window
        [ Gtk.containerBorderWidth := 5
        ]

    textView   <- Gtk.textViewNew
    textBuffer <- Gtk.get textView Gtk.textViewBuffer
    Gtk.set textView
        [ Gtk.textViewEditable      := False
        , Gtk.textViewCursorVisible := False
        ]

    hAdjust <- Gtk.textViewGetHadjustment textView
    vAdjust <- Gtk.textViewGetVadjustment textView
    scrolledWindow <- Gtk.scrolledWindowNew (Just hAdjust) (Just vAdjust)
    Gtk.set scrolledWindow
        [ Gtk.containerChild           := textView
        , Gtk.scrolledWindowShadowType := Gtk.ShadowIn
        ]

    vBox <- Gtk.vBoxNew False 5

    hBox <- Gtk.hBoxNew False 5
    Gtk.boxPackStart hBox vBox           Gtk.PackNatural 0
    Gtk.boxPackStart hBox scrolledWindow Gtk.PackGrow    0

    Gtk.set window
        [ Gtk.windowTitle         := title
        , Gtk.containerChild      := hBox
        , Gtk.windowDefaultWidth  := 600
        , Gtk.windowDefaultHeight := 400
        ]

    let __spinButton :: Text -> Double -> Cell Double
        __spinButton label stepX = Cell (do
            tmvar      <- STM.newEmptyTMVarIO
            let minX = fromIntegral (minBound :: Int)
            let maxX = fromIntegral (maxBound :: Int)
            spinButton_ <- Gtk.spinButtonNewWithRange minX maxX stepX
            Gtk.set spinButton_
                [ Gtk.spinButtonValue    := 0
                ]
            _  <- Gtk.onValueSpinned spinButton_ (do
                n <- Gtk.get spinButton_ Gtk.spinButtonValue
                STM.atomically (STM.putTMVar tmvar n) )

            frame <- Gtk.frameNew
            Gtk.set frame
                [ Gtk.containerChild := spinButton_
                , Gtk.frameLabel     := label
                ]

            Gtk.boxPackStart vBox frame Gtk.PackNatural 0
            Gtk.widgetShowAll vBox
            return (STM.takeTMVar tmvar, Fold.lastDef 0) )

    let __checkBox :: Text -> Cell Bool
        __checkBox label = Cell (do
            checkButton <- Gtk.checkButtonNewWithLabel label

            tmvar <- STM.newEmptyTMVarIO
            _     <- Gtk.on checkButton Gtk.toggled (do
                pressed <- Gtk.get checkButton Gtk.toggleButtonActive
                STM.atomically (STM.putTMVar tmvar pressed) )

            Gtk.boxPackStart vBox checkButton Gtk.PackNatural 0
            Gtk.widgetShowAll vBox
            return (STM.takeTMVar tmvar, Fold.lastDef False) )

    let __entry :: Text -> Cell Text
        __entry label = Cell (do
            entry_ <- Gtk.entryNew

            frame <- Gtk.frameNew
            Gtk.set frame
                [ Gtk.containerChild := entry_
                , Gtk.frameLabel     := label
                ]

            tmvar <- STM.newEmptyTMVarIO
            _     <- Gtk.on entry_ Gtk.editableChanged (do
                txt <- Gtk.get entry_ Gtk.entryText
                STM.atomically (STM.putTMVar tmvar txt) )

            Gtk.boxPackStart vBox frame Gtk.PackNatural 0
            Gtk.widgetShowAll frame
            return (STM.takeTMVar tmvar, Fold.lastDef Text.empty) )

    let __radioButton :: Show a => Text -> a -> [a] -> Cell a
        __radioButton label x xs = Cell (do
            tmvar <- STM.newEmptyTMVarIO

            vBoxRadio <- Gtk.vBoxNew False 5

            let makeButton f a = do
                    button <- f (show a)
                    Gtk.boxPackStart vBoxRadio button Gtk.PackNatural 0
                    _ <- Gtk.on button Gtk.toggled (do
                        mode <- Gtk.get button Gtk.toggleButtonMode
                        if mode
                            then STM.atomically (STM.putTMVar tmvar a)
                            else return () )
                    return button

            button <- makeButton Gtk.radioButtonNewWithLabel x
            mapM_ (makeButton (Gtk.radioButtonNewWithLabelFromWidget button)) xs

            frame <- Gtk.frameNew
            Gtk.set frame
                [ Gtk.containerChild := vBoxRadio
                , Gtk.frameLabel     := label
                ]
            Gtk.boxPackStart vBox frame Gtk.PackNatural 0
            Gtk.widgetShowAll frame
            return (STM.takeTMVar tmvar, Fold.lastDef x) )

    let control = Control
            { _checkBox    = __checkBox
            , _spinButton  = __spinButton
            , _entry       = __entry
            , _radioButton = __radioButton
            }

    doneTMVar <- STM.newEmptyTMVarIO

    let run :: Cell Text -> IO ()
        run (Cell m) = do
            (stm, Fold step begin done) <- Gtk.postGUISync m
            let loop x = do
                    let txt = done x
                    Gtk.postGUISync
                        (Gtk.set textBuffer [ Gtk.textBufferText := txt ])
                    let doneTransaction = do
                            STM.takeTMVar doneTMVar
                            return Nothing
                    me <- STM.atomically (doneTransaction <|> fmap pure stm)
                    case me of
                        Nothing -> return ()
                        Just e  -> loop (step x e)
            loop begin

    _ <- Gtk.on window Gtk.deleteEvent (liftIO (do
        STM.atomically (STM.putTMVar doneTMVar ())
        Gtk.mainQuit
        return False ))
    Gtk.widgetShowAll window
    Async.withAsync (run (k control)) (\a -> do
        Gtk.mainGUI
        Async.wait a )

-- | A check box that returns `True` if selected and `False` if unselected
checkBox
    :: Text
    -- ^ Label
    -> Updatable Bool
checkBox label = Updatable (\control -> _checkBox control label)

-- | A `Double` spin button
spinButton
    :: Text
    -- ^ Label
    -> Double
    -- ^ Step size
    -> Updatable Double
spinButton label x = Updatable (\control -> _spinButton control label x)

-- | A `Text` entry
entry
    :: Text
    -- ^ Label
    -> Updatable Text
entry label = Updatable (\control -> _entry control label)

-- | A control that selects from one or more mutually exclusive choices
radioButton
    :: Show a
    => Text
    -- ^ Label
    -> a
    -- ^ 1st choice (Default selection)
    -> [a]
    -- ^ Remaining choices
    -> Updatable a
radioButton label a0 as =
    Updatable (\control -> _radioButton control label a0 as)

-- | Convert a `Show`able value to `Text`
display :: Show a => a -> Text
display = Text.pack . show

-- $examples
--
-- Mortgage calculator:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Data.Text (Text)
-- > import Typed.Spreadsheet
-- > 
-- > payment :: Double -> Double -> Double -> Text
-- > payment mortgageAmount numberOfYears yearlyInterestRate
-- >     = display (mortgageAmount * (i * (1 + i) ^ n) / ((1 + i) ^ n - 1))
-- >   where
-- >     n = truncate (numberOfYears * 12)
-- >     i = yearlyInterestRate / 12 / 100
-- > 
-- > logic :: Updatable Text
-- > logic = payment <$> spinButton "Mortgage Amount"          1000
-- >                 <*> spinButton "Number of years"             1
-- >                 <*> spinButton "Yearly interest rate (%)"    0.01
-- > 
-- > main :: IO ()
-- > main = textUI "Mortgage payment" logic
--
-- Example input and output:
--
-- <<http://i.imgur.com/QimLicC.png Mortgage calculator program>>
--
-- Mad libs:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Data.Monoid ((<>))
-- > import Typed.Spreadsheet
-- > 
-- > noun = entry "Noun"
-- > 
-- > verb = entry "Verb"
-- > 
-- > adjective = entry "Adjective"
-- > 
-- > example =
-- >     "I want to " <> verb <> " every " <> noun <> " because they are so " <> adjective
-- > 
-- > main :: IO ()
-- > main = textUI "Mad libs" example
--
-- The above program works because the `Updatable` type implements `IsString`
-- and `Monoid`, so no `Applicative` operations are necessary
--
-- Example input and output:
--
-- <<http://i.imgur.com/k22An4Y.png Mad libs program>>
