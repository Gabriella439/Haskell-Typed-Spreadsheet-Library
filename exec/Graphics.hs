{-# LANGUAGE OverloadedStrings #-}

import Diagrams.Backend.Cairo (Cairo)
import Diagrams.Prelude
import Typed.Spreadsheet

data AColor = Red | Orange | Yellow | Green | Blue | Purple
    deriving (Enum, Bounded, Show)

toColor :: AColor -> Colour Double
toColor Red    = red
toColor Orange = orange
toColor Yellow = yellow
toColor Green  = green
toColor Blue   = blue
toColor Purple = purple

main :: IO ()
main = graphicalUI "Example program" logic
  where
    logic = combine <$> radioButton "Color"        Red [Orange .. Purple]
                    <*> spinButton  "Radius"       1
                    <*> spinButton  "X Coordinate" 1
                    <*> spinButton  "Y Coordinate" 1

    combine :: AColor -> Double -> Double -> Double -> Diagram Cairo
    combine color r x y =
        circle (r + 100) # fc (toColor color) # translate (r2 (x, -y))
