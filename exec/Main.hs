{-# LANGUAGE OverloadedStrings #-}

import Gtk.Composer

main :: IO ()
main = textUI "Example program" logic
  where
    logic = combine <$> checkBox   "a"
                    <*> spinButton "b" 1
                    <*> spinButton "c" 0.1
                    <*> entry      "d"

    combine a b c d = display (a, b + c, d)
