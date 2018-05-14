#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-14
   --package extensible
   --package yaml
-}

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TemplateHaskell #-}

import           Data.Extensible

import qualified Data.Yaml.TH as Yaml.TH (decodeFile)

type Config = Record
  '[ "name" :> String
   , "age"  :> Int
   ]

config :: Config
config = $$(Yaml.TH.decodeFile "config.yaml")

main :: IO ()
main = print config