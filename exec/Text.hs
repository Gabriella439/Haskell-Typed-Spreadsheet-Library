{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}

import Typed.Spreadsheet

main :: IO ()
main = textUI "Example program" $ do
    a <- checkBox   "a"
    b <- spinButton "b" 1
    c <- spinButton "c" 0.1
    d <- entry      "d"
    return (display (a, b + c, d))
