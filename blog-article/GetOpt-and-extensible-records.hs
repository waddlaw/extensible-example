#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
   --package lens
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

import Data.Extensible
import Data.Extensible.GetOpt

import Control.Lens (folded, (^.), (^?))

import System.Console.GetOpt (
  ArgDescr (NoArg, ReqArg), 
  ArgOrder (Permute), 
  OptDescr (Option), 
  getOpt, usageInfo)
import System.Environment (getArgs, getProgName)
import System.Exit (die)

data Options = Options
  { verbose :: Bool
  , extra   :: Maybe String
  }

defaultOptions :: Options
defaultOptions = Options
  { verbose = False
  , extra   = Nothing
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "v" ["verbose"] (NoArg  (\  o -> o { verbose = True   })      ) "verbose output"
  , Option "e" ["extra"]   (ReqArg (\e o -> o { extra   = Just e }) "ARG") "extra argument"
  ]

main :: IO ()
main = getOpt Permute options <$> getArgs >>= \case
  (fs, _, []) -> do
    let o = foldl (flip id) defaultOptions fs
    putStrLn $ "verbose: " ++ show (verbose o)
    putStrLn $ "extra: "   ++ show (extra o)
  (_, _, es) -> do
    name <- getProgName
    die $ unlines es ++ usageInfo name options

type OptionsRecord = RecordOf OptDescr'
  '[ "verbose" >: Int
   , "extra"   >: [String]
   ]

opts :: OptionsRecord
opts = #verbose @= optNoArg  "v" ["verbose"]       "verbose"
    <: #extra   @= optReqArg "e" ["extra"]   "ARG" "extra arguments"
    <: nil

main' :: IO ()
main' = withGetOpt "test" opts $ \r _args -> do
  putStrLn $ "verbose: " ++ show (r ^. #verbose > 0)
  putStrLn $ "extra: "   ++ show (r ^? #extra . folded)
