{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE PolyKinds            #-}
module Main where

data Nat = Zero | Succ Nat

type family Add (n :: Nat) (m :: Nat) :: Nat
type instance Add Zero m = m
type instance Add (Succ n) m = Add n (Succ m)

type family Sub (n :: Nat) (m :: Nat) :: Nat
type instance Sub n Zero = n
type instance Sub (Succ n) (Succ m) = Sub n m

type family Mul (n :: Nat) (m :: Nat) :: Nat
type instance Mul Zero m = Zero
type instance Mul (Succ Zero) m = m
type instance Mul (Succ (Succ n)) m = Add m (Mul (Succ n) m)

zero :: SNat Zero
zero = sing

one :: SNat (Succ Zero)
one = sing

two :: SNat (Succ (Succ Zero))
two = sing

-- |
-- 型レベルで 3 + 2 を行うように型を定義せよ
add :: SNat (Add (Succ (Succ (Succ Zero))) (Succ (Succ Zero)))
add = sing

-- |
-- 型レベルで 3 - 2 を行うように型を定義せよ
sub :: SNat (Sub (Succ (Succ (Succ Zero))) (Succ (Succ Zero)))
sub = sing

-- |
-- 型レベルで 3 * 2 を行うように型を定義せよ
mul :: SNat (Mul (Succ (Succ (Succ Zero))) (Succ (Succ Zero)))
mul = sing

main :: IO ()
main = do
  print zero
  print one
  print two
  print add
  print sub
  print mul

-- singleton
data SNat n where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

deriving instance Show (SNat n)

class Singleton (n :: Nat) where
  sing :: SNat n

instance Singleton Zero where
  sing = SZero

instance Singleton n => Singleton (Succ n) where
  sing = SSucc (sing :: SNat n)