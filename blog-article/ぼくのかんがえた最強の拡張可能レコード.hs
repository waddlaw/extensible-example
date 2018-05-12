#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-12
   --package extensible
   --package lens
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import           Data.Extensible

import           Control.Lens    (view, (^.))

type Person = Record
  '[ "personId" >: Int
   , "name"     >: String
   ]

type Address = Record
  '[ "personId" >: Int
   , "address"  >: String
   ]

getPersonId :: Associate "personId" Int xs => Record xs -> Int
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

main :: IO ()
main = do
  print fubuki
  print fubuki2
  print $ getPersonId fubuki
  print $ getPersonId fubuki2