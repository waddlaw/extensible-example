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

import           Data.Functor.Identity (runIdentity)
import           Data.Proxy            (Proxy (Proxy))
import           GHC.TypeLits          (KnownSymbol)

import           Data.Typeable         (Typeable, typeOf)

type Person = Record
  '[ "name" :> String
   , "age"  :> Int
   ]

person :: Person
person = #name @= "bigmoon"
      <: #age  @= 10
      <: nil

debug :: Person -> IO ()
debug = hfoldMapFor poly (print . fork show typeOf . runIdentity . getField)
  where
    poly = Proxy @ (ValueIs (And Show Typeable))
    fork f g x = (f x, g x)

main :: IO ()
main = do
  debug person
