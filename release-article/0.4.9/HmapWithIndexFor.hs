#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-12
   --package extensible-0.4.9
   --package data-default
   --package aeson
-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Data.Extensible
import           Data.Functor.Identity (Identity (Identity), runIdentity)
import           Data.Proxy            (Proxy (Proxy))

import           Data.Aeson            (ToJSON, toJSON, Value)
import           Data.Default          (Default, def)

type Person = Record
  '[ "name" :> String
   , "age"  :> Int
   ]

person :: Person
person = #name @= "bigmoon"
      <: #age  @= 10
      <: nil

toJSONRecord :: Forall (ValueIs ToJSON) xs => RecordOf Identity xs -> RecordOf (Const' Value) xs
toJSONRecord = hmapWithIndexFor poly $ \m -> Field . Const' . toJSON . runIdentity . getField
  where poly = Proxy @ (ValueIs ToJSON)

toDefaultRecord :: Forall (ValueIs Default) xs => Record xs -> Record xs
toDefaultRecord = hmapWithIndexFor poly $ \_m -> Field . Identity . def . runIdentity . getField
  where poly = Proxy @ (ValueIs Default)

main :: IO ()
main = do
  print $ person
  print $ toJSONRecord person
  print $ toDefaultRecord person
