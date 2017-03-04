{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}

import Typed.Spreadsheet

main :: IO ()
main = cellUI "Example program" $ do
    a <- checkBox   "a"
    b <- spinButton "b" 1
    c <- spinButton "c" 0.1
    d <- entry      "d"
    return
        [ ("a"    , display  a     )
        , ("b + c", display (b + c))
        , ("d"    , display  d     )
        ]
