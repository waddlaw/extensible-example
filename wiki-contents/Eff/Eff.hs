#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-14
   --package extensible
   --package mtl
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Extensible
import Data.Extensible.Effect.Default

import Control.Monad        (replicateM_)
import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Control.Monad.Writer (tell)
import Data.Monoid          (Sum (Sum))

type ExampleM = Eff
  '[ ReaderDef Int
   , StateDef  Int
   , WriterDef (Sum Int)
   ]

testEff :: ExampleM  ()
testEff = replicateM_ 100 $ do
  r <- ask
  s <- get
  tell (Sum s)
  put $! s + r

runExtensible :: ExampleM a -> ((a, Int), Sum Int)
runExtensible = leaveEff
              . runWriterDef
              . flip runStateDef  0
              . flip runReaderDef 1

main :: IO ()
main = print $ runExtensible testEff