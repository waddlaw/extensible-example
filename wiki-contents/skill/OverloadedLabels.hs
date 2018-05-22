#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-22
   --package extensible-0.4.9
   --package lens
   --package yaml
-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import           Data.Extensible

import           Control.Lens    ((^.))
import           Data.Proxy      (Proxy (Proxy))

import           Data.Yaml       (decodeFileEither, prettyPrintParseException)

type Package = Record '[ "default-extensions" >: [String] ]

defaultExtensions :: FieldOptic "default-extensions"
defaultExtensions = itemAssoc (Proxy @ "default-extensions")

readConfig :: FilePath -> IO Package
readConfig = fmap (either errMsg id) . decodeFileEither
  where
    errMsg = error . prettyPrintParseException

main :: IO ()
main = do
  pkgConfig <- readConfig "package.yaml"

  mapM_ putStrLn $ pkgConfig ^. defaultExtensions
