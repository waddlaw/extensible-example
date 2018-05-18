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

green :: Color
green = #rgb # (0, 255, 0)

white :: Color
white = #cmyk # (0, 0, 0, 0)

main :: IO ()
main = do
  print green
  print white