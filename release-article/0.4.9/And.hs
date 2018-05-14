#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-14
   --package extensible
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
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

debug :: Forall (ValueIs (And Show Typeable)) xs => Record xs -> IO ()
debug = hfoldMapFor c (print . fork id typeOf . runIdentity . getField)
  where
    c = Proxy @ (ValueIs (And Show Typeable))
    fork f g x = (f x, g x)

main :: IO ()
main = debug person
