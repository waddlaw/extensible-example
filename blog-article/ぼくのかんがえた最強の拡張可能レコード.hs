#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-12
   --package extensible
   --package lens
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts     #-}

import Data.Extensible

import Control.Lens          ((^.), view)

type Person = Record
  '[ "personId" :> Int
   , "name"     :> String
   ]

type Address = Record
  '[ "personId" :> Int
   , "address"  :> String
   ]

getPersonId :: (("personId" :> Int) ∈ xs) => Record xs -> Int
getPersonId = view #personId

fubuki :: Person
fubuki = #personId @= 1
      <: #name     @= "吹雪"
      <: nil

fubuki2 :: Person
fubuki2 = shrink
        $ #name     @= ("吹雪" :: String)
       <: #personId @= (1 :: Int)
       <: nil