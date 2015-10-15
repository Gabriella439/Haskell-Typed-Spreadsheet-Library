{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative.Cell

import qualified Data.Text as Text

main :: IO ()
main = runManaged (do
    (control, run) <- setup

    let f x y z = display (x, y, z)

    let result = f <$> bool control "Bool"
                   <*> int  control "Int"
                   <*> text control "Text"

    run result )

