#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
   --package th-lift-instances
   --package yaml
-}

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Extensible

import qualified Data.Yaml.TH as Yaml.TH (decodeFile)
import Instances.TH.Lift ()

type Config = Record
  '[ "name" :> String
   , "age"  :> Int
   ]

config :: Config
config = $$(Yaml.TH.decodeFile "config.yaml")

main :: IO ()
main = print config