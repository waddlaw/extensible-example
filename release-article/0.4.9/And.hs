#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

import Data.Extensible

import Data.Functor.Identity (runIdentity)
import Data.Typeable (Typeable, typeOf)

type Person = Record
  '[ "name" :> String
   , "age"  :> Int
   ]

person :: Person
person = #name @= "bigmoon"
      <: #age  @= 10
      <: nil

debug :: Forall (TargetIs (And Show Typeable)) xs => Record xs -> IO ()
debug = hfoldMapFor c (print . fork id typeOf . runIdentity . getField)
  where
    c = Proxy @(TargetIs (And Show Typeable))
    fork f g x = (f x, g x)

main :: IO ()
main = debug person
