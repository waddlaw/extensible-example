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

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import Data.Extensible

import Control.Monad.State (MonadState, get, put)

increment :: (Num a, MonadState a m) => m ()
increment = get >>= put . (+1)

test :: (Lookup xs "foo" ((,) String), Lookup xs "bar" ((,) String))
     => Eff xs ()
test = do
  tellEff #foo "Hello "
  tellEff #bar "foo"
  tellEff #foo "world"
  tellEff #bar "bar"

main :: IO ()
main = do
  print $ leaveEff $ runWriterEff @ "foo" $ runWriterEff @ "bar" test
  print $ leaveEff $ runWriterEff @ "bar" $ runWriterEff @ "foo" test
