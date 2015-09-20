{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
-- TODO: Remove this

module Control.Applicative.Cell (
      Cell
    , Controls(..)
    , spreadsheet
    , runManaged
    ) where

import Control.Applicative (Applicative(..), Alternative(..), liftA2)
import Control.Concurrent.STM (STM)
import Control.Foldl (Fold(..))
import Control.Monad.Managed (Managed, liftIO, managed, runManaged)
import Data.Text (Text)
import Lens.Micro (_Left, _Right)
import Graphics.UI.Gtk (AttrOp((:=)))

import qualified Control.Concurrent.STM   as STM
import qualified Control.Concurrent.Async as Async
import qualified Control.Foldl            as Fold
import qualified Control.Monad.Managed    as Managed
import qualified Data.Text                as Text
import qualified Graphics.UI.Gtk          as Gtk

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

data Controls = Controls
    { double :: Double -> Double -> Double -> Cell Double
    }

spreadsheet :: Managed (Controls, Cell Text -> Managed ())
spreadsheet = managed (\k -> do
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
        [ Gtk.windowTitle         := "Haskell Spreadsheet"
        , Gtk.containerChild      := hBox
        , Gtk.windowDefaultWidth  := 400
        , Gtk.windowDefaultHeight := 400
        ]

    let _double :: Double -> Double -> Double -> Cell Double
        _double minX maxX stepX = Cell (liftIO (do
            tmvar      <- STM.newEmptyTMVarIO
            spinButton <- Gtk.spinButtonNewWithRange minX maxX stepX
            _          <- Gtk.onValueSpinned spinButton (do
                n <- Gtk.get spinButton Gtk.spinButtonValue
                STM.atomically (STM.putTMVar tmvar n) )
            Gtk.boxPackStart vBox spinButton Gtk.PackNatural 0
            Gtk.widgetShowAll vBox
            return (STM.takeTMVar tmvar, Fold.lastDef 0) ))

    let controls = Controls
            { double = _double
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
    (Controls{..}, run) <- spreadsheet

    let f x y = Text.pack (show (x ** y))

    let result = f <$> double 0 100 1
                   <*> double 0 100 1
    run result )
