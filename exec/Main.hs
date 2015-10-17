{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative.Cell

main :: IO ()
main = runManaged (do
    (control, run) <- setup

    let f x y z = display (x, y, z)

    let result = f <$> bool control "Bool"
                   <*> int  control "Int"
                   <*> enum control "Enum" "Hey" ["Diddle", "Doo"]

    run result )
