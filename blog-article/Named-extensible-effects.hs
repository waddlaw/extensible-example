#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-14
   --package extensible
   --package mtl
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import Data.Extensible

import Control.Monad.State.Class (MonadState, get, put)

increment :: (Num a, MonadState a m) => m ()
increment = get >>= put . (+1)

test :: (Associate "foo" ((,) String) xs, Associate "bar" ((,) String) xs) => Eff xs ()
test = do
  tellEff #foo "Hello "
  tellEff #bar "foo"
  tellEff #foo "world"
  tellEff #bar "bar"

main :: IO ()
main = do
  print $ leaveEff $ runWriterEff @ "foo" $ runWriterEff @ "bar" test
  print $ leaveEff $ runWriterEff @ "bar" $ runWriterEff @ "foo" test