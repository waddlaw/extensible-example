#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
   --package text
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

import Data.Extensible

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (putStrLn)
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality

import Data.Kind

type Person = Record
  '[ "name" :> String
   , "age"  :> Int
   ]

type A =
  '[ "name" :> String
  , "age"  :> Int
  ]

person :: Person
person = #name @= "bigmoon"
      <: #age  @= 10
      <: nil

keys :: (IsString key, Forall (KeyIs KnownSymbol) xs) => Record xs -> [key]
keys (xs :: Record xs) = henumerateFor c xs' ((:) . stringKeyOf) []
  where
    c = Proxy @(KeyIs KnownSymbol)
    xs' :: Proxy xs
    xs' = Proxy

main :: IO ()
main = do
  mapM_ putStrLn $ keys person
  mapM_ TIO.putStrLn $ keys person
