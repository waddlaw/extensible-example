#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
   --package mtl
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Extensible
import Data.Extensible.Effect.Default

import Control.Monad.State (MonadState, get, put)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Monoid (Sum(Sum))
import Control.Monad (replicateM_)

testMTL :: (MonadReader Int m, MonadState Int m, MonadWriter (Sum Int) m)
        => m ()
testMTL = replicateM_ 100 $ do
  r <- ask
  s <- get
  tell (Sum s)
  put $! s + r

testEff ::  Eff '[ReaderDef Int, StateDef Int, WriterDef (Sum Int)] ()
testEff = replicateM_ 100 $ do
  r <- ask
  s <- get
  tell (Sum s)
  put $! s + r

runExtensible :: Eff '[ReaderDef Int, StateDef Int, WriterDef (Sum Int)] a
  -> ((a, Int), Sum Int)
runExtensible
  = leaveEff
  . runWriterDef
  . flip runStateDef 0
  . flip runReaderDef 1

main :: IO ()
main = print $ runExtensible testEff