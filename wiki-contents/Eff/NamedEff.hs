#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds  #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import Data.Extensible
import Data.Extensible.Effect.Default

type FooBarM xs =
  ( Lookup xs "foo" (WriterEff String)
  , Lookup xs "bar" (WriterEff String)
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
