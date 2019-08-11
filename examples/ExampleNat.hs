{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Main (main) where

import Data.Proxy

data Nat = Zero | Succ Nat

type family AddOne (n :: Nat) :: Nat
type instance AddOne n = Succ n

-- |
-- :t foo
-- foo :: Proxy ('Succ ('Succ 'Zero))
foo :: Proxy (AddOne (AddOne Zero))
foo = Proxy

type family Add (n :: Nat) (m :: Nat) :: Nat
type instance Add Zero m = m
-- type instance Add n Zero = n
type instance Add (Succ n) m = Add n (Succ m)

-- |
-- :t bar
-- bar :: Proxy ('Succ ('Succ ('Succ 'Zero)))
bar :: Proxy (Add (Succ Zero) (Succ (Succ Zero)))
bar = Proxy

main :: IO ()
main = do
  