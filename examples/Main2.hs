#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
   --package lens
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE OverloadedLabels #-}

import Data.Extensible
import Data.Proxy
import Data.Typeable
import Control.Lens hiding ((:>))

data MyData
  = MyDataA
  | MyDataB

foo :: Proxy "abc"
foo = Proxy :: Proxy (AssocValue ("num" >: "abc"))

baz :: RecordOf Proxy '["name" >: "guchi", "age" >: "18"]
baz = #name @= ()
  <: #age @= ()
  <: nil

proxyAssocValue :: proxy kv -> Proxy (AssocValue kv)
proxyAssocValue _ = Proxy

f :: Proxy  -> Comp Proxy AssocValue
f _ = Comp Proxy

foo2 :: RecordOf Proxy '["name" >: "guchi", "age" >: "18"]
foo2 = #num <@=> 42
  <: #str <@=> "foo"
  <: nil

-- hmap (Comp . getField) baz ::  (Comp Proxy AssocValue) :* '["name" >: "guchi", "age" >: "18"]