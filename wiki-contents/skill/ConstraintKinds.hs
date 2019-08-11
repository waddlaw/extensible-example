#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
   --package lens
-}

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import Data.Extensible

import Control.Lens ((^.))

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

type NameAge xs =
  ( Lookup xs "name" String
  , Lookup xs "age"  Int
  )

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
  print $ getNameAge p1
  print $ getNameAge p2
