# typed-spreadsheet v1.1.4

`typed-spreadsheet` provides a typed and composable API for building
spreadsheets.  This differs from traditional spreadsheets in a few important
ways:

* you specify the relationship between inputs and outputs at compile time, not
  runtime, so that the relationship can be type-checked
* inputs of different types have different controls; for example, a `Bool` input
  creates a checkbox and a `Double` input creates a spin button
* you can only output a single value; you simulate multiple outputs by emitting
  a tuple of values

# Quick Start

This project includes two example executables that you can build and tweak to
test drive the library.  To clone, build, and run the executable just follow
these commands depending on your operating system:

## OS X

```bash
$ # Installs the Gtk+ library
$ brew install gtk

$ # Creates a local copy of this repository
$ git clone https://github.com/Gabriella439/Haskell-Typed-Spreadsheet-Library.git

$ # Builds the executables
$ stack build --stack-yaml=osx.yaml --install-ghc

$ # Runs the text output example
$ stack exec  --stack-yaml=osx.yaml typed-spreadsheet-example-text

$ # Runs the graphical example
$ stack exec  --stack-yaml=osx.yaml typed-spreadsheet-example-graphics
```

## Debian

These instructions will also probably work on other Linux distributions derived
from Debian like Ubuntu or Mint:

```bash
$ # Install the Gtk+ 2.0 development headers
$ sudo apt-get install libgtk2.0-dev

$ # Creates a local copy of this repository
$ git clone https://github.com/Gabriella439/Haskell-Typed-Spreadsheet-Library.git

$ # Builds the executables
$ stack build --install-ghc

$ # Runs the text output example
$ stack exec typed-spreadsheet-example-text

$ # Runs the graphical example
$ stack exec typed-spreadsheet-example-graphics
```

## Other operating systems

If you would like to contribute build instructions for other operating systems,
please submit a pull request.

## Examples

The [executable code](https://github.com/Gabriella439/Haskell-Typed-Spreadsheet-Library/blob/master/exec/Text.hs)
for first example is short:

```haskell
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
```

... and translates to a spreadsheet with all inputs on the left-hand side and
the output on the right-hand side:

![](http://i.imgur.com/TTxgSwN.png)

You can also output updatable diagrams built using the `diagrams` library, such
as [in this example](https://github.com/Gabriella439/Haskell-Typed-Spreadsheet-Library/blob/master/exec/Graphics.hs):

```haskell
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
    color <- radioButton      "Color"        Red [Orange .. Purple]
    r     <- spinButtonAt 100 "Radius"       1
    x     <- spinButton       "X Coordinate" 1
    y     <- spinButton       "Y Coordinate" 1
    return (circle r # fc (toColor color) # translate (r2 (x, y)))
```

This produces a canvas that colors, resizes, and moves a circle in response to
user input:

![](http://i.imgur.com/ddYoG46.png)

To learn more about the library, read the
[documentation on Hackage](http://hackage.haskell.org/package/typed-spreadsheet/docs/Typed-Spreadsheet.html).

# Additional examples

Mortgage calculator:

```haskell
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}

import Typed.Spreadsheet

main :: IO ()
main = textUI "Mortgage payment" $ do
  mortgageAmount     <- spinButton "Mortgage Amount"          1000
  numberOfYears      <- spinButton "Number of years"             1
  yearlyInterestRate <- spinButton "Yearly interest rate (%)"    0.01
  let n = truncate (numberOfYears * 12)
  let i = yearlyInterestRate / 12 / 100
  return ("Monthly payment: $" <> display (mortgageAmount * (i * (1 + i) ^ n) / ((1 + i) ^ n - 1)))
```

Example input and output:

![Mortgage calculator program](http://i.imgur.com/nvRZ9HC.png)

Mad libs:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Typed.Spreadsheet

noun = entry "Noun"

verb = entry "Verb"

adjective = entry "Adjective"

example =
    "I want to " <> verb <> " every " <> noun <> " because they are so " <> adjective

main :: IO ()
main = textUI "Mad libs" example
```

Example input and output:

![Mad libs program](http://i.imgur.com/k22An4Y.png)

Sinusoid plot:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Diagrams.Prelude
import Typed.Spreadsheet

main :: IO ()
main = graphicalUI "Example program" $ do
    amplitude <- spinButtonAt 50  "Amplitude (Pixels)"   0.1
    frequency <- spinButtonAt 0.1 "Frequency (Pixels⁻¹)" 0.001
    phase     <- spinButtonAt 90  "Phase (Degrees)"      1

    let axes = arrowBetween (p2 (0, 0)) (p2 ( 100,    0))
            <> arrowBetween (p2 (0, 0)) (p2 (-100,    0))
            <> arrowBetween (p2 (0, 0)) (p2 (   0,  100))
            <> arrowBetween (p2 (0, 0)) (p2 (   0, -100))

    let f x = amplitude * cos (frequency * x + phase * pi / 180)

    let points = map (\x -> p2 (x, f x)) [-100, -99 .. 100]

    return (strokeP (fromVertices points) <> axes)
```

Example input and output:

![Sinusoid plot](http://i.imgur.com/ueF0w7U.png)

Factor diagram:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Diagrams.Prelude
import Diagrams.TwoD.Factorization (factorDiagram')
import Typed.Spreadsheet

main :: IO ()
main = graphicalUI "Factor diagram" $ do
    x <- spinButtonAt 3 "Factor #1" 1
    y <- spinButtonAt 3 "Factor #2" 1
    z <- spinButtonAt 3 "Factor #3" 1
    return (factorDiagram' [truncate x, truncate y, truncate z] # scale 10)
```

Example input and output:

![Factor diagram](http://i.imgur.com/eMvMtKk.png)

# How to contribute

The easiest way to contribute is to add new controls for user input.

If you are feeling particularly adventurous, you can work on adding a `diagrams`
output option instead of a text-based output.

## Development Status

[![Build Status](https://travis-ci.org/Gabriella439/Haskell-Typed-Spreadsheet-Library.png)](https://travis-ci.org/Gabriella439/Haskell-Typed-Spreadsheet-Library)

The high-level API for this library is unlikely to change.  I expect most of
the volatility early on will be in how much configuration for controls to expose
to end users.  For example, controls could be generalized to permit stateful
operations, but it's not clear if that additional complexity is worth it.

There might be new features added later such as `diagrams` output or new
controls, but I expect those additions will not require any breaking changes to
the API.

## LICENSE (BSD 3-Clause)

Copyright (c) 2014 Gabriella Gonzalez
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the name of Gabriella Gonzalez nor the names of other contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
