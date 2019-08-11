#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
   --package mtl
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell  #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import Data.Extensible
import Data.Extensible.Effect.Default

import Control.Monad.State (MonadState, modify)

increment :: (Num a, MonadState a m) => m ()
increment = modify (+1)

test :: ( Lookup xs "foo" (WriterEff String)
        , Lookup xs "bar" (WriterEff String)
        ) => Eff xs ()
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
