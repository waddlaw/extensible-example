#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-14
   --package extensible
   --package mtl
-}

{-# LANGUAGE DataKinds #-}

import Data.Extensible
import Data.Extensible.Effect.Default

import Control.Monad.State            (MonadState, modify)

increment :: (Num a, MonadState a m) => m ()
increment = modify (+1)

main :: IO ()
main = print $ leaveEff $ runStateDef increment 0