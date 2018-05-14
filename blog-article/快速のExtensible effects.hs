#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-14
   --package extensible
   --package mtl
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell  #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import           Data.Extensible
import           Data.Extensible.Effect.Default

import           Control.Monad.State            (MonadState, modify)

increment :: (Num a, MonadState a m) => m ()
increment = modify (+1)

test :: (Associate "foo" (WriterEff String) xs, Associate "bar" (WriterEff String) xs) => Eff xs ()
test = do
  tellEff #foo "Hello "
  tellEff #bar "hoge"
  tellEff #foo "world"
  tellEff #bar "fuga"

decEffects [d|
  data Blah a b x where
    Blah :: Int -> a -> Blah a b b
  |]

main :: IO ()
main = do
  print $ leaveEff $ runStateDef increment 0
  print $ leaveEff $ runWriterEff @ "foo" $ runWriterEff @ "bar" test
  print $ leaveEff $ runWriterEff @ "bar" $ runWriterEff @ "foo" test
