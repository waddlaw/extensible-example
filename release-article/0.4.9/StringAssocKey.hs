#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-14
   --package extensible
   --package text
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

import           Data.Extensible

import           Data.Proxy      (Proxy (Proxy))
import           Data.String     (IsString)
import           Data.Text       (Text)
import qualified Data.Text.IO    as TIO (putStrLn)
import           GHC.TypeLits    (KnownSymbol)

type Person = Record
  '[ "name" :> String
   , "age"  :> Int
   ]

person :: Person
person = #name @= "bigmoon"
      <: #age  @= 10
      <: nil

keys :: (IsString key, Forall (KeyIs KnownSymbol) xs) => proxy xs -> [key]
keys xs = henumerateFor (Proxy @ (KeyIs KnownSymbol)) xs ((:) . stringAssocKey) []

main :: IO ()
main = do
  mapM_ putStrLn $ keys person
  mapM_ TIO.putStrLn $ keys person
