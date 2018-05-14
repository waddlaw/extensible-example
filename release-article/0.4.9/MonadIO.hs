#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-14
   --package extensible
   --package resourcet
   --package conduit
   --package bytestring
-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import           Data.Extensible
import           Data.Extensible.Effect.Default

import           Data.Proxy                     (Proxy (Proxy))

import           Conduit
import           Control.Monad.Trans.Resource   (ResourceT, liftResourceT)
import           Data.ByteString                (ByteString)
import           GHC.IO.Handle                  (Handle, hClose)
import           System.IO                      (IOMode (ReadMode), openFile)

type ExampleM = Eff '[ "IO" >: ResourceT IO ]

main :: IO ()
main = runResourceT . retractEff . runConduit $
  bracketP (openFile "data.csv" ReadMode) hClose $ \handle ->
    (sourceHandle handle :: ConduitT i ByteString ExampleM ()) .| printC

instance (Associate "IO" (ResourceT IO) xs) => MonadResource (Eff xs) where
  liftResourceT = liftEff (Proxy :: Proxy "IO")
