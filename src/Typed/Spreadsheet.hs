{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE CPP              #-}

-- | The following program:
--
-- > {-# LANGUAGE ApplicativeDo     #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Typed.Spreadsheet
-- > 
-- > main :: IO ()
-- > main = textUI "Example program" $ do
-- >     a <- checkBox   "a"
-- >     b <- spinButton "b" 1
-- >     c <- spinButton "c" 0.1
-- >     d <- entry      "d"
-- >     return (display (a, b + c, d))
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
-- This library also supports graphical output, like in the following program:
--
-- > {-# LANGUAGE ApplicativeDo     #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Diagrams.Prelude
-- > import Typed.Spreadsheet
-- > 
-- > data AColor = Red | Orange | Yellow | Green | Blue | Purple
-- >     deriving (Enum, Bounded, Show)
-- > 
-- > toColor :: AColor -> Colour Double
-- > toColor Red    = red
-- > toColor Orange = orange
-- > toColor Yellow = yellow
-- > toColor Green  = green
-- > toColor Blue   = blue
-- > toColor Purple = purple
-- > 
-- > main :: IO ()
-- > main = graphicalUI "Example program" $ do
-- >     color <- radioButton      "Color"        Red [Orange .. Purple]
-- >     r     <- spinButtonAt 100 "Radius"       1
-- >     x     <- spinButton       "X Coordinate" 1
-- >     y     <- spinButton       "Y Coordinate" 1
-- >     return (circle r # fc (toColor color) # translate (r2 (x, y)))
--
-- This produces a canvas that colors, resizes, and moves a circle in response
-- to user input:
--
-- <<http://i.imgur.com/ddYoG46.png Graphical user interface>>
--
-- The general workflow for this library is:
--
-- * You build primitive `Updatable` values using `checkBox`, `spinButton`,
--   `entry`, or `radioButton`, each of which corresponds to a control on the
--   left panel of the user interface
-- * Combine `Updatable` values using @ApplicativeDo@ notation.  Composite values
--   update whenever one of their substituent values update
-- * You consume an @(`Updatable` `Text`)@ value using `textUI`, which displays
--   the continuously updating value in the right panel of the user interface
--
-- You can get started quickly by cloning and building this project:
--
-- > $ git clone https://github.com/Gabriel439/Haskell-Typed-Spreadsheet-Library.git
-- > $ stack build --install-ghc             # Builds the executable
-- > $ stack exec typed-spreadsheet-example  # Runs the executable
--
-- ... or if you are using OS X, then build the project using:
--
-- > $ stack --stack-yaml=osx.yaml build --install-ghc
--
-- That project includes the code for the above examples in the @exec/@
-- subdirectory.  Just modify that file and rebuild to play with the example.
--
-- NOTE: You must compile your program with the @-threaded@ flag.  The example
-- project takes care of this.
--
-- See the \"Examples\" section at the bottom of this module for more examples.

module Typed.Spreadsheet (
    -- * Types
      Updatable
    , textUI
    , cellUI
    , graphicalUI
    , ui

    -- * Controls
    , checkBox
    , spinButton
    , entry
    , radioButton

    -- * Controls with Defaults
    , checkBoxAt
    , spinButtonAt
    , hscale
    , hscaleAt
    , hscaleWithRange
    , vscale
    , vscaleAt
    , vscaleWithRange
    , entryAt

    -- * Utilities
    , display

    -- * Examples
    -- $examples
    ) where

import Control.Applicative
import Control.Concurrent.STM (STM)
import Control.Foldl (Fold(..))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import Data.Text (Text)
import Diagrams.Backend.Cairo (Cairo)
import Diagrams.BoundingBox (boundingBox, boxExtents, boxCenter)
import Diagrams.Prelude (Diagram, r2, reflectY, translate, (#), V2(..), Point(..), scale, negated)
import Lens.Micro (_Left, _Right)
import Graphics.UI.Gtk (AttrOp((:=)))

import qualified Control.Concurrent
import qualified Control.Concurrent.STM   as STM
import qualified Control.Concurrent.Async
import qualified Control.Foldl
import qualified Data.Text
import qualified Diagrams.Backend.Gtk
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

            fold  = do
                f <- Control.Foldl.handles _Left foldF
                x <- Control.Foldl.handles _Right foldX
                return (f x)

-- | An updatable input value
data Updatable a = Updatable (Control -> Cell a)

instance Functor Updatable where
    fmap f (Updatable m) = Updatable (fmap (fmap f) m)

instance Applicative Updatable where
    pure a = Updatable (pure (pure a))

    Updatable mf <*> Updatable mx = Updatable (liftA2 (<*>) mf mx)

#if MIN_VERSION_base(4,11,0)
instance Semigroup a => Semigroup (Updatable a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Updatable a) where
    mempty = pure mempty

#else
instance Monoid a => Monoid (Updatable a) where
    mempty = pure mempty

    mappend = liftA2 mappend
#endif


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
    { _checkBoxAt      :: Bool -> Text -> Cell Bool
    , _spinButtonAt    :: Double -> Text -> Double -> Cell Double
    , _hscaleWithRange :: Double -> Double -> Double -> Text -> Double -> Cell Double
    , _vscaleWithRange :: Double -> Double -> Double -> Text -> Double -> Cell Double
    , _entryAt         :: Text -> Text -> Cell Text
    , _radioButton     :: forall a . Show a => Text -> a -> [a] -> Cell a
    }

-- | Build a `Text`-based user interface
textUI
    :: Text
    -- ^ Window title
    -> Updatable Text
    -- ^ Program logic
    -> IO ()
textUI = ui textSetup processTextEvent
  where
    textSetup :: Gtk.HBox -> IO Gtk.TextBuffer
    textSetup hBox = do
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
            [ Gtk.containerChild                 := textView
            , Gtk.scrolledWindowShadowType       := Gtk.ShadowIn
            , Gtk.scrolledWindowHscrollbarPolicy := Gtk.PolicyAutomatic
            , Gtk.scrolledWindowVscrollbarPolicy := Gtk.PolicyAutomatic
            ]
        Gtk.boxPackStart hBox scrolledWindow Gtk.PackGrow 0
        return textBuffer

    processTextEvent :: Gtk.TextBuffer -> Text -> IO ()
    processTextEvent textBuffer txt =
        Gtk.set textBuffer [ Gtk.textBufferText := txt ]

-- | Build a cell-based user interface
cellUI
    :: Text
    -- ^ Window title
    -> Updatable [(Text, Text)]
    -- ^ Program logic
    -> IO ()
cellUI = ui cellSetup processCellEvent
  where
    cellSetup :: Gtk.HBox -> IO Gtk.VBox
    cellSetup hBox = do
        vbox <- Gtk.vBoxNew False 5
        Gtk.boxPackStart hBox vbox Gtk.PackGrow 0
        return vbox

    processCellEvent :: Gtk.VBox -> [(Text, Text)] -> IO ()
    processCellEvent vbox keyVals = do
        cells <- Gtk.containerGetChildren vbox
        mapM_ (Gtk.containerRemove vbox) cells

        let createCell (key, val) = do
                textView   <- Gtk.textViewNew
                textBuffer <- Gtk.get textView Gtk.textViewBuffer
                Gtk.set textView
                    [ Gtk.textViewEditable      := False
                    , Gtk.textViewCursorVisible := False
                    ]
                Gtk.set textBuffer [ Gtk.textBufferText := val ]
                hAdjust <- Gtk.textViewGetHadjustment textView
                vAdjust <- Gtk.textViewGetVadjustment textView
                scrolledWindow <- do
                    Gtk.scrolledWindowNew (Just hAdjust) (Just vAdjust)
                Gtk.set scrolledWindow
                    [ Gtk.containerChild :=
                        textView
                    , Gtk.scrolledWindowShadowType :=
                        Gtk.ShadowIn
                    , Gtk.scrolledWindowHscrollbarPolicy :=
                        Gtk.PolicyAutomatic
                    , Gtk.scrolledWindowVscrollbarPolicy :=
                        Gtk.PolicyAutomatic
                    ]
                frame <- Gtk.frameNew
                Gtk.set frame
                    [ Gtk.containerChild := scrolledWindow
                    , Gtk.frameLabel     := key
                    ]
                Gtk.boxPackStart vbox frame Gtk.PackNatural 0
        mapM_ createCell keyVals
        Gtk.widgetShowAll vbox

-- | Build a `Diagram`-based user interface
graphicalUI
    :: Text
    -- ^ Window title
    -> Updatable (Diagram Cairo)
    -- ^ Program logic
    -> IO ()
graphicalUI = ui setupGraphical processGraphicalEvent
  where
    setupGraphical :: Gtk.HBox -> IO Gtk.DrawingArea
    setupGraphical hBox = do
        drawingArea <- Gtk.drawingAreaNew
        Gtk.boxPackStart hBox drawingArea Gtk.PackGrow 0
        return drawingArea

    processGraphicalEvent :: Gtk.DrawingArea -> Diagram Cairo -> IO ()
    processGraphicalEvent drawingArea diagram = do
        drawWindow <- Gtk.widgetGetDrawWindow drawingArea
        (w, h) <- Gtk.widgetGetSize drawingArea
        let w' = fromIntegral w
        let h' = fromIntegral h
        let boundsD = boundingBox diagram
        let V2 bw bh = boxExtents boundsD
        let border = max 1 (sqrt (w' ^ (2 :: Int) + h' ^ (2 :: Int)) * 0.05)
        let nearZero x = abs x < 1e-15
        let scaleW =
              if nearZero bw
                then 1
                else max 1 (w' - border) / bw
        let scaleH =
              if nearZero bh
              then 1
              else max 1 (h' - border) / bh
        let scaleUniform
              | scaleW <= 1 && scaleH >= 1 = scaleW
              | scaleH <= 1 && scaleW >= 1 = scaleH
              | otherwise = min scaleH scaleW
        let P boundsCenter = fromMaybe (P (V2 0 0)) (boxCenter boundsD)
        let diagram' = diagram
                     # translate (negated boundsCenter)
                     # scale scaleUniform
                     # reflectY
                     # translate (r2 (w' / 2, h' / 2))
        Diagrams.Backend.Gtk.renderToGtk drawWindow diagram'

-- | Underlying function for building custom user interfaces
ui  :: (Gtk.HBox -> IO resource)
    -- ^ Acquire initial resource
    -> (resource -> event -> IO ())
    -- ^ Callback function to process each event
    -> Text
    -- ^ Window title
    -> Updatable event
    -- ^ Event stream
    -> IO ()
ui setup process title (Updatable k) = do
    _ <- Gtk.initGUI

    window <- Gtk.windowNew
    Gtk.set window
        [ Gtk.containerBorderWidth := 5
        ]

    vBox <- Gtk.vBoxNew False 5

    hBox <- Gtk.hBoxNew False 5
    Gtk.boxPackStart hBox vBox Gtk.PackNatural 0
    a    <- setup hBox

    Gtk.set window
        [ Gtk.windowTitle         := title
        , Gtk.containerChild      := hBox
        , Gtk.windowDefaultWidth  := 600
        , Gtk.windowDefaultHeight := 400
        ]

    let __spinButtonAt :: Double -> Text -> Double -> Cell Double
        __spinButtonAt s0 label stepX = Cell (do
            tmvar      <- STM.newEmptyTMVarIO
            let minX = fromIntegral (minBound :: Int)
            let maxX = fromIntegral (maxBound :: Int)
            spinButton_ <- Gtk.spinButtonNewWithRange minX maxX stepX
            Gtk.set spinButton_
                [ Gtk.spinButtonValue    := s0
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
            return (STM.takeTMVar tmvar, Control.Foldl.lastDef s0) )

    let __hscaleWithRange :: Double -> Double -> Double -> Text -> Double -> Cell Double
        __hscaleWithRange minY maxY s0 label stepY = Cell (do
            tmvar      <- STM.newEmptyTMVarIO
            slider <- Gtk.hScaleNewWithRange minY maxY stepY
            Gtk.set slider
                [ Gtk.rangeValue    := s0
                ]
            _  <- Gtk.onRangeValueChanged slider (do
                n <- Gtk.get slider Gtk.rangeValue
                STM.atomically (STM.putTMVar tmvar n) )
            frame <- Gtk.frameNew
            Gtk.set frame
                [ Gtk.containerChild := slider
                , Gtk.frameLabel     := label
                ]
            Gtk.boxPackStart vBox frame Gtk.PackNatural 0
            Gtk.widgetShowAll vBox
            return (STM.takeTMVar tmvar, Control.Foldl.lastDef s0) )

    let __vscaleWithRange :: Double -> Double -> Double -> Text -> Double -> Cell Double
        __vscaleWithRange minY maxY s0 label stepY = Cell (do
            tmvar      <- STM.newEmptyTMVarIO
            slider <- Gtk.vScaleNewWithRange minY maxY stepY
            Gtk.set slider
                [ Gtk.rangeValue    := (-s0)
                ]
            _  <- Gtk.onRangeValueChanged slider (do
                n <- Gtk.get slider Gtk.rangeValue
                STM.atomically (STM.putTMVar tmvar (-n)) )
            frame <- Gtk.frameNew
            Gtk.set frame
                [ Gtk.containerChild := slider
                , Gtk.frameLabel     := label
                ]
            Gtk.boxPackStart hBox frame Gtk.PackNatural 0
            Gtk.widgetShowAll hBox
            return (STM.takeTMVar tmvar, Control.Foldl.lastDef s0) )

    let __checkBoxAt :: Bool -> Text -> Cell Bool
        __checkBoxAt s0 label = Cell (do
            checkButton <- Gtk.checkButtonNewWithLabel label

            Gtk.set checkButton [ Gtk.toggleButtonActive := s0 ]
            tmvar <- STM.newEmptyTMVarIO
            _     <- Gtk.on checkButton Gtk.toggled (do
                pressed <- Gtk.get checkButton Gtk.toggleButtonActive
                STM.atomically (STM.putTMVar tmvar pressed) )

            Gtk.boxPackStart vBox checkButton Gtk.PackNatural 0
            Gtk.widgetShowAll vBox
            return (STM.takeTMVar tmvar, Control.Foldl.lastDef s0) )

    let __entryAt :: Text -> Text -> Cell Text
        __entryAt s0 label = Cell (do
            entry_ <- Gtk.entryNew

            frame <- Gtk.frameNew
            Gtk.set frame
                [ Gtk.containerChild := entry_
                , Gtk.frameLabel     := label
                ]
            Gtk.set entry_ [ Gtk.entryText := s0 ]

            tmvar <- STM.newEmptyTMVarIO
            _     <- Gtk.on entry_ Gtk.editableChanged (do
                txt <- Gtk.get entry_ Gtk.entryText
                STM.atomically (STM.putTMVar tmvar txt) )

            Gtk.boxPackStart vBox frame Gtk.PackNatural 0
            Gtk.widgetShowAll frame
            return (STM.takeTMVar tmvar, Control.Foldl.lastDef s0) )

    let __radioButton :: Show a => Text -> a -> [a] -> Cell a
        __radioButton label x xs = Cell (do
            tmvar <- STM.newEmptyTMVarIO

            vBoxRadio <- Gtk.vBoxNew False 5

            let makeButton f y = do
                    button <- f (show y)
                    Gtk.boxPackStart vBoxRadio button Gtk.PackNatural 0
                    _ <- Gtk.on button Gtk.toggled (do
                        active <- Gtk.get button Gtk.toggleButtonActive
                        if active
                            then STM.atomically (STM.putTMVar tmvar y)
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
            return (STM.takeTMVar tmvar, Control.Foldl.lastDef x) )

    let control = Control
            { _checkBoxAt      = __checkBoxAt
            , _spinButtonAt    = __spinButtonAt
            , _hscaleWithRange = __hscaleWithRange
            , _vscaleWithRange = __vscaleWithRange
            , _entryAt         = __entryAt
            , _radioButton     = __radioButton
            }

    doneTMVar <- STM.newEmptyTMVarIO

    let run (Cell m) = do
            (stm, Fold step begin done) <- Gtk.postGUISync m
            -- I don't know why this delay is necessary for diagrams output
            Control.Concurrent.threadDelay 200000
            let loop x = do
                    let b = done x
                    Gtk.postGUISync (process a b)
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
    Control.Concurrent.Async.withAsync (run (k control)) (\s -> do
        Gtk.mainGUI
        Control.Concurrent.Async.wait s )

-- | A check box that returns `True` if selected and `False` if unselected
checkBox
    :: Text
    -- ^ Label
    -> Updatable Bool
checkBox = checkBoxAt False

-- | A `Double` spin button
spinButton
    :: Text
    -- ^ Label
    -> Double
    -- ^ Step size
    -> Updatable Double
spinButton = spinButtonAt 0

-- | A `Double` horizontal slider
hscale
    :: Text
    -- ^ Label
    -> Double
    -- ^ Step size
    -> Updatable Double
hscale = hscaleAt 0

-- | A `Double` vertical slider
vscale
    :: Text
    -- ^ Label
    -> Double
    -- ^ Step size
    -> Updatable Double
vscale = vscaleAt 0

-- | A `Text` entry
entry
    :: Text
    -- ^ Label
    -> Updatable Text
entry = entryAt Data.Text.empty

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

-- | Same as `checkBox` except that you can specify the initial state
checkBoxAt
    :: Bool
    -- ^ Initial state 
    -> Text
    -- ^ Label
    -> Updatable Bool
checkBoxAt s0 label =
    Updatable (\control -> _checkBoxAt control s0 label)

-- | Same as `spinButton` except that you can specify the initial state
spinButtonAt
    :: Double
    -- ^ Initial state
    -> Text
    -- ^ Label
    -> Double
    -- ^ Step size
    -> Updatable Double
spinButtonAt s0 label x =
    Updatable (\control -> _spinButtonAt control s0 label x)

-- | Same as `hscaleButton` except that you can specify the initial state
hscaleAt
    :: Double
    -- ^ Initial state
    -> Text
    -- ^ Label
    -> Double
    -- ^ Step size
    -> Updatable Double
hscaleAt = hscaleWithRange (fromIntegral (minBound :: Int)) (fromIntegral (maxBound :: Int))

-- | Same as `hscaleButton` except that you can specify the range and initial state
hscaleWithRange
    :: Double
    -- ^ Minimum value
    -> Double
    -- ^ Maximum value
    -> Double
    -- ^ Initial state
    -> Text
    -- ^ Label
    -> Double
    -- ^ Step size
    -> Updatable Double
hscaleWithRange b0 b1 s0 label x =
    Updatable (\control -> _hscaleWithRange control b0 b1 s0 label x)

-- | Same as `vscaleButton` except that you can specify the initial state
vscaleAt
    :: Double
    -- ^ Initial state
    -> Text
    -- ^ Label
    -> Double
    -- ^ Step size
    -> Updatable Double
vscaleAt = vscaleWithRange (fromIntegral (minBound :: Int)) (fromIntegral (maxBound :: Int))

-- | Same as `vscaleButton` except that you can specify the range and initial state
vscaleWithRange
    :: Double
    -- ^ Minimum value
    -> Double
    -- ^ Maximum value
    -> Double
    -- ^ Initial state
    -> Text
    -- ^ Label
    -> Double
    -- ^ Step size
    -> Updatable Double
vscaleWithRange b0 b1 s0 label x =
    Updatable (\control -> _vscaleWithRange control b0 b1 s0 label x)

-- | Same as `entry` except that you can specify the initial state
entryAt
    :: Text
    -- ^ Initial state
    -> Text
    -- ^ Label
    -> Updatable Text
entryAt s0 label = Updatable (\control -> _entryAt control s0 label)

-- | Convert a `Show`able value to `Text`
display :: Show a => a -> Text
display = Data.Text.pack . show

-- $examples
--
-- Mortgage calculator:
--
-- > {-# LANGUAGE ApplicativeDo     #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Typed.Spreadsheet
-- > 
-- > main :: IO ()
-- > main = textUI "Mortgage payment" $ do
-- >   mortgageAmount     <- spinButton "Mortgage Amount"          1000
-- >   numberOfYears      <- spinButton "Number of years"             1
-- >   yearlyInterestRate <- spinButton "Yearly interest rate (%)"    0.01
-- >   let n = truncate (numberOfYears * 12)
-- >   let i = yearlyInterestRate / 12 / 100
-- >   return ("Monthly payment: $" <> display (mortgageAmount * (i * (1 + i) ^ n) / ((1 + i) ^ n - 1)))
--
-- Example input and output:
--
-- <<http://i.imgur.com/nvRZ9HC.png Mortgage calculator program>>
--
-- Mad libs:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
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
--
-- Sinusoid plot:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Diagrams.Prelude
-- > import Typed.Spreadsheet
-- > 
-- > main :: IO ()
-- > main = graphicalUI "Example program" $ do
-- >     amplitude <- spinButtonAt 50  "Amplitude (Pixels)"   0.1
-- >     frequency <- spinButtonAt 0.1 "Frequency (Pixels⁻¹)" 0.001
-- >     phase     <- spinButtonAt 90  "Phase (Degrees)"      1
-- > 
-- >     let axes = arrowBetween (p2 (0, 0)) (p2 ( 100,    0))
-- >             <> arrowBetween (p2 (0, 0)) (p2 (-100,    0))
-- >             <> arrowBetween (p2 (0, 0)) (p2 (   0,  100))
-- >             <> arrowBetween (p2 (0, 0)) (p2 (   0, -100))
-- > 
-- >     let f x = amplitude * cos (frequency * x + phase * pi / 180)
-- > 
-- >     let points = map (\x -> p2 (x, f x)) [-100, -99 .. 100]
-- > 
-- >     return (strokeP (fromVertices points) <> axes)
--
-- Example input and output:
--
-- <<http://i.imgur.com/ueF0w7U.png Sinusoid plot>>
--
-- Factor diagram:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Diagrams.Prelude
-- > import Diagrams.TwoD.Factorization (factorDiagram')
-- > import Typed.Spreadsheet
-- > 
-- > main :: IO ()
-- > main = graphicalUI "Factor diagram" $ do
-- >     x <- spinButtonAt 3 "Factor #1" 1
-- >     y <- spinButtonAt 3 "Factor #2" 1
-- >     z <- spinButtonAt 3 "Factor #3" 1
-- >     return (factorDiagram' [truncate x, truncate y, truncate z] # scale 10)
--
-- Example input and output:
--
-- <<http://i.imgur.com/eMvMtKk.png Factor diagram>>
