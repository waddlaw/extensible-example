#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
   --package resourcet
   --package conduit
   --package bytestring
-}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

import Data.Extensible
import Data.Extensible.Effect.Default

import Conduit
import Data.ByteString (ByteString)
import GHC.IO.Handle (hClose)
import System.IO (IOMode (ReadMode), openFile)

type ExampleM = Eff '[ "IO" >: ResourceT IO ]

main :: IO ()
main = runResourceT . retractEff . runConduit $
  bracketP (openFile "data.csv" ReadMode) hClose $ \handle ->
    (sourceHandle handle :: ConduitT i ByteString ExampleM ()) .| stdoutC
