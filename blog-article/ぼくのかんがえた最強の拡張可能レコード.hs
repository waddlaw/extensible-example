#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
   --package lens
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import Data.Extensible

import Control.Lens (view, (^.))

type Person = Record
  '[ "personId" >: Int
   , "name"     >: String
   ]

type Address = Record
  '[ "personId" >: Int
   , "address"  >: String
   ]

getPersonId :: Lookup xs "personId" Int => Record xs -> Int
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