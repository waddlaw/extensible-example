#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
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

import Data.Extensible

import Control.Applicative (Const (..))
import Control.Lens (view)
import Data.Functor.Identity (Identity (Identity))
import Data.Aeson (ToJSON, Value, toJSON)
import Data.Default (Default, def)

type Person = Record
  '[ "name" :> String
   , "age"  :> Int
   ]

person :: Person
person = #name @= "bigmoon"
      <: #age  @= 10
      <: nil

toJSONRecord :: Forall (TargetIs ToJSON) xs => RecordOf Identity xs -> RecordOf (Const Value) xs
toJSONRecord = hmapWithIndexFor c $ \m -> Field . Const . toJSON . view _Wrapper
  where
    c = Proxy @(TargetIs ToJSON)

toDefaultRecord :: Forall (TargetIs Default) xs => Record xs -> Record xs
toDefaultRecord = hmapWithIndexFor c $ \_m -> Field . Identity . def . view _Wrapper
  where
    c = Proxy @ (TargetIs Default)

main :: IO ()
main = do
  print person
  print $ toJSONRecord person
  print $ toDefaultRecord person
