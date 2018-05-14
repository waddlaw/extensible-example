#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-14
   --package extensible
   --package prettyprinter
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

import           Data.Extensible

import           Data.Text.Prettyprint.Doc             (pretty)
import           Data.Text.Prettyprint.Doc.Render.Text (putDoc)

type Person = Record
  '[ "name" :> String
   , "age"  :> Int
   ]

person :: Person
person = #name @= "bigmoon"
      <: #age  @= 10
      <: nil

main :: IO ()
main = do
  print $ pretty person
  print $ pretty [person, person]

  putDoc $ pretty person
  putDoc $ pretty [person, person]
