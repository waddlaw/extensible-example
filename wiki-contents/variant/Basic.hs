#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
   --package lens
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

import Data.Extensible

import Control.Lens ((#))

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