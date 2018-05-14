#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-14
   --package extensible
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds  #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import           Data.Extensible
import           Data.Extensible.Effect.Default

type FooBarM xs =
  ( Associate "foo" (WriterEff String) xs
  , Associate "bar" (WriterEff String) xs
  )

test :: FooBarM xs => Eff xs ()
test = do
  tellEff #foo "Hello "
  tellEff #bar "hoge"
  tellEff #foo "world"
  tellEff #bar "fuga"

main :: IO ()
main = do
  print $ leaveEff $ runWriterEff @ "foo" $ runWriterEff @ "bar" test
  print $ leaveEff $ runWriterEff @ "bar" $ runWriterEff @ "foo" test
