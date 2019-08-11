#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package singletons
-}

{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExplicitForAll       #-}

module Paper where

import Prelude hiding (span)
import Data.Singletons.TH (promote)
import Data.Singletons.Prelude ((:$))

-- 2.1 Datakinds
data OperatingSystem (unixLike :: Bool) where
  MacOS   :: OperatingSystem True
  Linux   :: OperatingSystem True
  Windows :: OperatingSystem False

-- 2.2 Type families
data Nat = Zero | Succ Nat
type family IsZero n where
  IsZero Zero      = True
  IsZero (Succ n ) = False

-- 2.3 Kind polymorphism
type family Length (list :: [a]) :: Nat where
  Length '[] = Zero
  Length (x : xs) = Succ (Length xs)

-- 3. Promoting functions
span :: (a -> Bool) -> [a] -> ([a], [a])
span _ xs@[] = (xs, xs)
span p xs@(x:xs')
  | p x = let (ys, zs) = span p xs' in (x:ys, zs)
  | otherwise = ([], xs)

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq [] = []
nubBy eq (x:xs) = x:nubBy eq (filter (\y -> not (eq x y)) xs)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy eq (x:xs) = (x:ys) : groupBy eq zs
  where (ys, zs) = span (eq x) xs

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) =
  let rs = mapMaybe f xs in
  case f x of
    Nothing -> rs
    Just r -> r:rs

$(promote [d|
  map :: (a -> b) -> [a] -> [b]
  map _ [] = []
  map f (x:xs) = f x : map f xs
  |])

-- 3.1 A longer example
-- |
-- >>> reorderBy (==) [1..10] [2,4,6,8]
-- [2,4,6,8,1,3,5,7,9,10]
reorderBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> [a]
reorderBy _ x [] = x
reorderBy eq x (h:t) =
  case extract eq h x of
    (lst, Nothing) -> reorderBy eq lst t
    (lst, Just elt) -> elt : (reorderBy eq lst t)

extract :: (a -> a -> Bool) -> a -> [a] -> ([a], Maybe a)
extract _ _ [] = ([], Nothing)
extract eq s (h:t)
  | s `eq` h = (t, Just s)
  | otherwise = let (resList, resVal) = extract eq s t
                in (h : resList, resVal)