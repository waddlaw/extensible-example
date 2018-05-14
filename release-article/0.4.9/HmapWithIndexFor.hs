#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-14
   --package extensible
   --package data-default
   --package aeson
   --package lens
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

import           Data.Extensible

import           Control.Lens          (view)
import           Data.Functor.Identity (Identity (Identity))
import           Data.Proxy            (Proxy (Proxy))

import           Data.Aeson            (ToJSON, Value, toJSON)
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
toJSONRecord = hmapWithIndexFor poly $ \m -> Field . Const' . toJSON . view _Wrapper
  where poly = Proxy @ (ValueIs ToJSON)

toDefaultRecord :: Forall (ValueIs Default) xs => Record xs -> Record xs
toDefaultRecord = hmapWithIndexFor poly $ \_m -> Field . Identity . def . view _Wrapper
  where poly = Proxy @ (ValueIs Default)

main :: IO ()
main = do
  print person
  print $ toJSONRecord person
  print $ toDefaultRecord person
