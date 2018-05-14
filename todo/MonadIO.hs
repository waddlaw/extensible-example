#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-14
   --package extensible
   --package resourcet
   --package conduit
   --package bytestring
-}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

import           Data.Extensible
import           Data.Extensible.Effect.Default

import           Conduit
import           Control.Monad.Trans.Resource   (ResourceT)
import           Data.ByteString                (ByteString)

type Bad = Eff '[ "IO" >: ResourceT IO ]

type Good = ResourceT IO

bad :: ConduitT ByteString ByteString Bad ()
bad = undefined

good :: ConduitT ByteString ByteString Good ()
good = undefined

main :: IO ()
main = runConduitRes
     $ sourceFile "input"
    -- .| bad    -- コンパイルエラー
    .| good
    .| sinkFile "output"