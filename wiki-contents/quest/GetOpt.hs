#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-18
   --package extensible-0.4.9
   --package lens
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

import Data.Extensible
import Data.Extensible.GetOpt

import Control.Lens (folded, (^.), (^?))

type Options = RecordOf OptDescr'
  '[ "verbose" >: Int
   , "extra"   >: [String]
   ]

opts :: Options
opts = #verbose @= optNoArg  "v" ["verbose"]       "verbose"
    <: #extra   @= optReqArg "e" ["extra"]   "ARG" "extra arguments"
    <: nil

main :: IO ()
main = withGetOpt "test" opts $ \r _args -> do
  putStrLn $ "verbose: " ++ show (r ^. #verbose > 0)
  putStrLn $ "extra: "   ++ show (r ^? #extra . folded)
