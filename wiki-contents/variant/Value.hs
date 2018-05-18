#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-18
   --package extensible-0.4.9
   --package lens
-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

import Data.Extensible

import Control.Lens    (( # ))

type Color = Variant
  '[ "rgb"  >: (Int, Int, Int)
   , "cmyk" >: (Int, Int, Int, Int)
   ]

color1 :: Variant '[ "rgb" >: (Int, Int, Int) ]
color1 = #rgb # (0, 0, 0)

color2 :: Color
color2 = #rgb # (0, 0, 0)

main :: IO ()
main = do
  print color1
  print color2