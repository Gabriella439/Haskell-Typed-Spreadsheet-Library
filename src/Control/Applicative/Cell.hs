{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Example usage:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Control.Applicative.Cell
-- > 
-- > main :: IO ()
-- > main = runManaged (do
-- >     (control, run) <- setup
-- > 
-- >     let combine x y z = display (x, y, z)
-- >     
-- >     let result = combine <$> bool control "Bool"
-- >                          <*> int  control "Int"
-- >                          <*> text control "Text"
-- >     
-- >     run result )

module Control.Applicative.Cell (
    -- * Types
      Cell
    , Control

    -- * Controls
    , bool
    , int
    , double
    , text
    , enum

    -- * Setup
    , setup
    , runManaged

    -- * Utilities
    , display
    ) where

import Control.Applicative
import Control.Concurrent.STM (STM)
import Control.Foldl (Fold(..))
import Control.Monad.Managed (Managed, liftIO, managed, runManaged)
import Data.Text (Text)
import Lens.Micro (_Left, _Right)
import Graphics.UI.Gtk (AttrOp((:=)))

import qualified Control.Concurrent.STM   as STM
import qualified Control.Concurrent.Async as Async
import qualified Control.Foldl            as Fold
import qualified Data.Text                as Text
import qualified Graphics.UI.Gtk          as Gtk

-- | An updatable value
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

{-| Use a `Control` to obtain updatable `bool`, `double` or `text` input
    `Cell`s
-}
data Control = Control
    { _bool   :: Text -> Cell Bool
    , _double :: Text -> Double -> Cell Double
    , _text   :: Text -> Cell Text
    , _enum   :: forall a . Show a => Text -> a -> [a] -> Cell a
    }

-- | A `Bool` control
bool
    :: Control
    -> Text
    -- ^ Label
    -> Cell Bool
bool = _bool

-- | An `Int` control
int
    :: Control
    -- ^
    -> Text
    -- ^ Label
    -> Cell Int
int control label = fmap truncate (double control label 1)

-- | A `Double` control
double
    :: Control
    -- ^
    -> Text
    -- ^ Label
    -> Double
    -- ^ Step size
    -> Cell Double
double = _double

-- | A `Text` control
text
    :: Control
    -- ^
    -> Text
    -- ^ Label
    -> Cell Text
text = _text

-- | A control that selects from a list of values
enum
    :: Show a
    => Control
    -- ^
    -> Text
    -- ^ Label
    -> a
    -- ^ 1st value (Default selection)
    -> [a]
    -- ^ Remaining values
    -> Cell a
enum = _enum

{-| Acquire two values:

    * The first value is a `Control`, which you can use to create `Cell`s
    * The second value is a function which builds a spreadsheet from a
      @(`Cell` `Text`)@
-}
setup :: Managed (Control, Cell Text -> Managed ())
setup = managed (\k -> do
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
        [ Gtk.windowTitle         := "Haskell Playground"
        , Gtk.containerChild      := hBox
        , Gtk.windowDefaultWidth  := 600
        , Gtk.windowDefaultHeight := 400
        ]

    let __double :: Text -> Double -> Cell Double
        __double label stepX = Cell (do
            tmvar      <- STM.newEmptyTMVarIO
            let minX = fromIntegral (minBound :: Int)
            let maxX = fromIntegral (maxBound :: Int)
            spinButton <- Gtk.spinButtonNewWithRange minX maxX stepX
            Gtk.set spinButton
                [ Gtk.spinButtonValue    := 0
                ]
            _  <- Gtk.onValueSpinned spinButton (do
                n <- Gtk.get spinButton Gtk.spinButtonValue
                STM.atomically (STM.putTMVar tmvar n) )

            frame <- Gtk.frameNew
            Gtk.set frame
                [ Gtk.containerChild := spinButton
                , Gtk.frameLabel     := label
                ]

            Gtk.boxPackStart vBox frame Gtk.PackNatural 0
            Gtk.widgetShowAll vBox
            return (STM.takeTMVar tmvar, Fold.lastDef 0) )

    let __bool :: Text -> Cell Bool
        __bool label = Cell (do
            checkButton <- Gtk.checkButtonNewWithLabel label

            tmvar <- STM.newEmptyTMVarIO
            _     <- Gtk.on checkButton Gtk.toggled (do
                pressed <- Gtk.get checkButton Gtk.toggleButtonActive
                STM.atomically (STM.putTMVar tmvar pressed) )

            Gtk.boxPackStart vBox checkButton Gtk.PackNatural 0
            Gtk.widgetShowAll vBox
            return (STM.takeTMVar tmvar, Fold.lastDef False) )

    let __text :: Text -> Cell Text
        __text label = Cell (do
            entry <- Gtk.entryNew

            frame <- Gtk.frameNew
            Gtk.set frame
                [ Gtk.containerChild := entry
                , Gtk.frameLabel     := label
                ]

            tmvar <- STM.newEmptyTMVarIO
            _     <- Gtk.on entry Gtk.editableChanged (do
                txt <- Gtk.get entry Gtk.entryText
                STM.atomically (STM.putTMVar tmvar txt) )

            Gtk.boxPackStart vBox frame Gtk.PackNatural 0
            Gtk.widgetShowAll frame
            return (STM.takeTMVar tmvar, Fold.lastDef Text.empty) )

    let __enum :: Show a => Text -> a -> [a] -> Cell a
        __enum label x xs = Cell (do
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

    let controls = Control
            { _bool    = __bool
            , _double  = __double
            , _text    = __text
            , _enum    = __enum
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
    Async.withAsync (k (controls, liftIO . run)) (\a -> do
        Gtk.mainGUI
        Async.wait a ) )

-- | Convert a `Show`able value to `Text`
display :: Show a => a -> Text
display = Text.pack . show
