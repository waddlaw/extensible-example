#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-12
   --package extensible
   --package random
   --package transformers
-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import           Data.Extensible

import           Control.Monad.Trans.Class (lift)
import           Data.Proxy                (Proxy (Proxy))
import           System.Random             (randomIO)

data Rec = Rec
  { foo :: String
  , bar :: Int
  , baz :: Double
  , qux :: Bool
  } deriving Show

makeRec :: IO Rec
makeRec = do
  foo <- getLine
  bar <- length <$> getLine
  baz <- readLn
  qux <- randomIO
  return Rec{..}

---

type Fields = '[ "foo2" >: String
               , "bar2" >: Int
               , "baz2" >: Double
               , "qux2" >: Bool
               ]

type Rec2 = Field Identity :* Fields

mkField "foo2 bar2 baz2 qux2"

class MakeRec kv where
  make :: proxy kv -> IO (AssocValue kv)

instance MakeRec ("foo2" >: String) where
  make _ = getLine

instance MakeRec ("bar2" >: Int) where
  make _ = length <$> getLine

instance MakeRec ("baz2" >: Double) where
  make _ = readLn

instance MakeRec ("qux2" >: Bool) where
  make _ = randomIO

makeRec2 :: IO Rec2
makeRec2 = hgenerateFor (Proxy @ MakeRec) (\m -> Field . pure <$> make m)

---

class MakeRec2 kv where
  make2 :: proxy kv -> TangleT (Field Identity) Fields IO (AssocValue kv)

instance MakeRec2 ("foo2" :> String) where
  make2 _ = lift getLine

instance MakeRec2 ("bar2" :> Int) where
  make2 _ = lift $ length <$> getLine

instance MakeRec2 ("baz2" :> Double) where
  make2 _ = lift readLn

instance MakeRec2 ("qux2" :> Bool) where
  make2 _ = do
    str <- lasso foo2
    x   <- lasso baz2
    return $ str == show x

tangles :: Comp (TangleT (Field Identity) Fields IO) (Field Identity) :* Fields
tangles = htabulateFor (Proxy @ MakeRec2) (\m -> Comp $ Field . pure <$> make2 m)

makeRec3 :: IO (Record Fields)
makeRec3 = runTangles tangles (wrench nil)

---

main :: IO ()
main = do
  makeRec  >>= print
  makeRec2 >>= print
  makeRec3 >>= print
