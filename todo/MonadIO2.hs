#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-14
   --package extensible
   --package monad-logger
-}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Extensible
import           Data.Extensible.Effect.Default

import           Control.Monad.Logger           (LoggingT, runStdoutLoggingT, logDebug)

type ExampleM = Eff '[ "Logger" >: LoggingT IO ]

run :: ExampleM a -> IO a
run = runStdoutLoggingT . retractEff

main :: IO ()
main = do
  res <- run $ do
    $(logDebug) "This is a debug log message"
  print res