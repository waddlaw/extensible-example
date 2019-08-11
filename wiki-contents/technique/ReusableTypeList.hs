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

type RGBField  = '[ "rgb"  >: (Int, Int, Int) ]
type CMYKField = '[ "cmyk" >: (Int, Int, Int, Int) ]

type RGB  = Variant RGBField
type CMYK = Variant CMYKField

type Color = Variant (RGBField ++ CMYKField)

green :: RGB
green = #rgb # (0, 255, 0)

white :: CMYK
white = #cmyk # (0, 0, 0, 0)

green2 :: Color
green2 = #rgb # (0, 255, 0)

white2 :: Color
white2 = #cmyk # (0, 0, 0, 0)

main :: IO ()
main = do
  print green
  print white
  print green2
  print white2

  print (spread green :: Color)
  print (spread white :: Color)