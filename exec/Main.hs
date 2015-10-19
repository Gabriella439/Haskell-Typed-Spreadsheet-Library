{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative.Cell

main :: IO ()
main = textUI (\control ->
    let combine a b c d = display (a, b + c, d)
    
    in combine <$> checkBox   control "a"
               <*> spinButton control "b" 1
               <*> spinButton control "c" 0.1
               <*> entry      control "d" )
