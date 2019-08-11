#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
   --package text
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

import Data.Extensible

import Data.Text
import GHC.TypeLits

newtype AssocValue' a = AssocValue' { getValue :: Proxy (TargetOf a) }

baz :: RecordOf Proxy '["name" >: "guchi", "age" >: "18"]
baz = #name @= () <: #age @= () <: nil

proxy :: Proxy (KeyTargetAre KnownSymbol KnownSymbol)
proxy = Proxy

main :: IO ()
main = do
  -- print $ hfoldMapFor proxy ((: []) . symbolVal . getValue) $ hmap (AssocValue' . getField) baz
  print $ hfoldMapFor proxy ((: []) . symbolVal . getField) baz