#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
   --package lens
   --package aeson
   --package bytestring
   --package cassava
   --package random
   --package text
   --package transformers
   --package vector
-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

import Data.Extensible
import Data.Extensible.Effect.Default

import Control.Lens ((^.))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
-- import Control.Monad.Trans.Error (throwError)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B (intercalate, readFile)
import Data.Csv (Header, decodeByName)
import Data.String(fromString)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V (head)
import System.Environment (getArgs)
import System.Random (randomRIO)

type Rec = Record Fields
type Fields =
  '[ "hoge1" >: String
   , "hoge2" >: Bool
   , "hoge3" >: Int
   ]

makeRec :: IO Rec
makeRec = do
  hoge1 <- getLine
  hoge3 <- randomRIO (0, 2 * length hoge1)
  pure $ #hoge1 @= hoge1
      <: #hoge2 @= (length hoge1 <= hoge3)
      <: #hoge3 @= hoge3
      <: nil

---

makeRec' :: IO Rec
makeRec' = runTangles tangles (wrench nil)

type FieldI = Field Identity

tangles :: Fields :& Comp (TangleT Fields FieldI IO) FieldI
tangles = htabulateFor c (Comp . fmap (Field . pure) . make)
  where c = Proxy @ MakeRec

class MakeRec kv where
  make :: proxy kv -> TangleT Fields FieldI IO (TargetOf kv)

instance MakeRec ("hoge1" >: String) where
  make _ = lift getLine

instance MakeRec ("hoge2" >: Bool) where
  make _ = (<=) <$> (length <$> lasso #hoge1)
                <*> lasso #hoge3

instance MakeRec ("hoge3" >: Int) where
  make _ = do
    ml <- length <$> lasso #hoge1
    lift $ randomRIO (0, 2 * ml)

----

type Time = Text

type Log = Record LogFields
type LogFields =
  '[ "path"    >: FilePath
   , "time"    >: Time
   , "code"    >: Int
   , "message" >: Text
   ]

type LogCsv = Record CsvFields
type CsvFields =
  '[ "time" >: Time
   , "info" >: ByteString
   ]

type Info = Record
  '[ "code"    >: Int
   , "message" >: Text
   ]

type EIO = Eff
  '[ EitherDef String
   , "IO" >: IO
   ]

runEIO :: EIO a -> IO (Either String a)
runEIO = retractEff . runEitherDef

header :: ByteString
header = B.intercalate "," $ keys xs
  where xs = Proxy @ CsvFields

keys :: (Forall (KeyIs KnownSymbol) xs) => proxy xs -> [ByteString]
keys xs = henumerateFor c xs ((:) . stringKeyOf) []
  where c = Proxy @ (KeyIs KnownSymbol)

----

main :: IO ()
main = do
  r1 <- makeRec
  print r1

  r2 <- makeRec'
  print r2

  ----

  -- extensible にインスタンスが追加されたら直す
  -- result <- runEIO $ do
  --   (path:_) <- liftIO getArgs

  --   csv <- mappend (header `mappend` "\n") <$> liftIO (B.readFile path)
  --   log' <- V.head . snd <$> either throwError pure (decodeByName csv) :: EIO LogCsv

  --   info <- either throwError pure (eitherDecode $ log' ^. #info) :: EIO Info
  --   pure $ #path @= path <: #time @= (log' ^. #time) <: info

  -- either error print result