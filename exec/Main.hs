{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative.Cell

main :: IO ()
main = runManaged (do
    (control, run) <- setup

    let combine a b c d = display (a, b + c, d)
    
    let result = combine <$> checkBox   control "a"
                         <*> spinButton control "b" 1
                         <*> spinButton control "c" 0.1
                         <*> entry      control "d"
    
    run result )
