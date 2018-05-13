#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-12
   --package extensible-0.4.9
   --package lens
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import Data.Extensible

import Control.Lens          ((^.), view)

type Person1 = Record
  '[ "name"     :> String
   , "age"      :> Int
   , "favorite" :> String
   ]

type Person2 = Record
  '[ "name"    :> String
   , "age"     :> Int
   , "address" :> String
   ]

getName :: Associate "name" Int xs => Record xs -> Int
getName = view #name

type NameAge xs = And (Associate "name" String) (Associate "age" Int) xs

getNameAge :: NameAge xs => Record xs -> (String, Int)
getNameAge r = (r ^. #name, r ^. #age)

p1 :: Person1
p1 = #name     @= "bigmoon"
  <: #age      @= 10
  <: #favorite @= "watch"
  <: nil

p2 :: Person2
p2 = #name    @= "bigmoon"
  <: #age     @= 10
  <: #address @= "Nagoya"
  <: nil

main :: IO ()
main = do
  print p1
  print p2

  print $ getNameAge p1
  print $ getNameAge p2