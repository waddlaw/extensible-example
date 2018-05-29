#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-29
   --package extensible-0.4.9
   --package lens
-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE TypeOperators     #-}

import Data.Extensible

import Control.Lens ((#))

import Data.Char (toUpper)

type ABC = Variant ["a" >: String, "b" >: String, "c" >: String]
type AB  = Variant ["a" >: String, "b" >: String]

f :: IncludeAssoc ["a" >: String, "b" >: String, "c" >: String] xs
  => Variant xs -> String
f = matchField $ shrinkAssoc
  $  #a @= id
  <: #b @= map toUpper
  <: #c @= (\c -> mconcat [":", c, ":"])
  <: nil

abc :: ABC
abc = #c # "c"

ab :: AB
ab = #b # "b"

main :: IO ()
main = do
  print abc
  print ab
  print $ f abc
  print $ f ab
