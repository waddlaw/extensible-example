#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
   --package mtl
-}

{-# LANGUAGE DataKinds #-}

import Data.Extensible
import Data.Extensible.Effect.Default

import Control.Monad.State (MonadState, modify)

increment :: (Num a, MonadState a m) => m ()
increment = modify (+1)

main :: IO ()
main = print $ leaveEff $ runStateDef increment 0