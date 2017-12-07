{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}

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
main = graphicalUI "Example program" $ do
    color <- radioButton                   "Color"        Red [Orange .. Purple]
    r     <- spinButtonAt    100           "Radius"       1
    x     <- hscaleWithRange (-200) 200 0  "X Coordinate" 10
    y     <- spinButton                    "Y Coordinate" 1
    return (circle r # fc (toColor color) # translate (r2 (x, y)))
