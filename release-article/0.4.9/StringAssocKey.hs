#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-12
   --package extensible-0.4.9
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

import           Data.Extensible

import           Data.Proxy      (Proxy (Proxy))
import           GHC.TypeLits    (KnownSymbol)

type Person = Record
  '[ "name" :> String
   , "age"  :> Int
   ]

person :: Person
person = #name @= "bigmoon"
      <: #age  @= 10
      <: nil

keys :: Forall (KeyIs KnownSymbol) xs => proxy xs -> [String]
keys xs = henumerateFor (Proxy @ (KeyIs KnownSymbol)) xs ((:) . stringAssocKey) []

main :: IO ()
main = print $ keys person
