#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
   --package lens
-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Ex01 where

import Data.Extensible

import Control.Lens ((#))
import Data.Functor.Identity

type A = '[ "a" >: Int ]
type B = '[ "b" >: Bool ]

class C kv where
  f :: proxy kv -> TargetOf kv -> String

instance C ("a" >: Int) where
  f _ = show

instance C ("b" >: Bool) where
  f _ = show

a :: Variant A
a = #a # 1

b :: Variant B
b = #b # True

type AB = A ++ B

aba :: Variant AB
aba = #a # 1

abb :: Variant AB
abb = #b # True

run :: Forall C xs => Variant xs -> String
run = matchField (htabulateFor c $ \m -> Field (Match $ f m . runIdentity))
  where
    c = Proxy @C