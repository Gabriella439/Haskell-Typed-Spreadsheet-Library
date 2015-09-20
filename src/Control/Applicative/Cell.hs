{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE OverloadedStrings         #-}
-- TODO: Remove this

module Control.Applicative.Cell (
      Cell
    , Control
    , int
    , double
    , text
    , setup
    , runManaged
    ) where

import Control.Applicative (Applicative(..), Alternative(..), liftA2)
import Control.Concurrent.STM (STM)
import Control.Foldl (Fold(..))
import Control.Monad.Managed (Managed, liftIO, managed, runManaged)
import Data.String (IsString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Lens.Micro (_Left, _Right)
import Graphics.UI.Gtk (AttrOp((:=)))

import qualified Control.Concurrent.STM   as STM
import qualified Control.Concurrent.Async as Async
import qualified Control.Foldl            as Fold
import qualified Control.Monad.Managed    as Managed
import qualified Data.Text                as Text
import qualified Graphics.UI.Gtk          as Gtk

newtype Label = Label { getLabel :: Text } deriving (IsString)

data Cell a = forall e . Cell (Managed (STM e, Fold e a))

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

data Control = Control
    { _int    :: Text -> Cell Int
    , _double :: Text -> Double -> Cell Double
    , _text   :: Text -> Cell Text
    }

int
    :: Control
    -- ^
    -> Text
    -- ^ Label
    -> Cell Int
int = _int

double
    :: Control
    -- ^
    -> Text
    -- ^ Label
    -> Double
    -- ^ Step size
    -> Cell Double
double = _double

text
    :: Control
    -- ^
    -> Text
    -- ^ Label
    -> Cell Text
text = _text

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
        , Gtk.textViewLeftMargin    := 5
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
        [ Gtk.windowTitle         := ("Haskell Spreadsheet" :: Text)
        , Gtk.containerChild      := hBox
        , Gtk.windowDefaultWidth  := 600
        , Gtk.windowDefaultHeight := 400
        ]

    let __double :: Text -> Double -> Cell Double
        __double label stepX = Cell (liftIO (do
            tmvar      <- STM.newEmptyTMVarIO
            let minX = fromIntegral (minBound :: Int)
            let maxX = fromIntegral (maxBound :: Int)
            spinButton <- Gtk.spinButtonNewWithRange minX maxX stepX
            Gtk.set spinButton
                [ Gtk.widgetMarginLeft   := 1
                , Gtk.widgetMarginRight  := 1
                , Gtk.widgetMarginBottom := 1
                , Gtk.spinButtonValue    := 0
                ]
            _          <- Gtk.onValueSpinned spinButton (do
                n <- Gtk.get spinButton Gtk.spinButtonValue
                STM.atomically (STM.putTMVar tmvar n) )

            frame <- Gtk.frameNew
            Gtk.set frame
                [ Gtk.containerChild := spinButton
                , Gtk.frameLabel     := label
                ]

            Gtk.boxPackStart vBox frame Gtk.PackNatural 0
            Gtk.widgetShowAll vBox
            return (STM.takeTMVar tmvar, Fold.lastDef 0) ))

    let __int :: Text -> Cell Int
        __int label = fmap truncate (__double label 1)

    let __text :: Text -> Cell Text
        __text label = Cell (liftIO (do
            entry <- Gtk.entryNew
            Gtk.set entry
                [ Gtk.widgetMarginLeft   := 1
                , Gtk.widgetMarginRight  := 1
                , Gtk.widgetMarginBottom := 1
                ]

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
            return (STM.takeTMVar tmvar, Fold.lastDef Text.empty) ))

    let controls = Control
            { _int     = __int
            , _double  = __double
            , _text    = __text
            }

    doneTMVar <- STM.newEmptyTMVarIO

    let run :: Cell Text -> Managed ()
        run (Cell m) = do
            (stm, Fold step begin done) <- m
            Managed.liftIO (do
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
                loop begin )

    _ <- Gtk.on window Gtk.deleteEvent (liftIO (do
        STM.atomically (STM.putTMVar doneTMVar ())
        Gtk.mainQuit
        return False ))
    Async.withAsync (k (controls, run)) (\a -> do
        Gtk.widgetShowAll window
        Gtk.mainGUI
        Async.wait a ) )

-- TODO: Remove this
main = runManaged (do
    (control, run) <- setup

    let f x y = Text.pack (show x) <> " " <> y

    let result = f <$> double control "Count" 0.01
                   <*> text control "Noun"

    run result )
