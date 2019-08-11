{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Comp where

import Data.Proxy

data Nat = Zero | Succ Nat

type family AddOne (n :: Nat) :: Nat
type instance AddOne n = Succ n

type family Comp (g :: k2 -> k3) (f :: k1 -> k2) (a :: k1) :: k3
type instance Comp g f a = g (f a)

type family Apply (f :: k1 -> k2) (a :: k1) :: k2
-- type instance Apply f a = f a
-- type instance Apply AddOne n = Succ n

-- foo :: Comp AddOne AddOne Zero :: Nat
-- foo :: AddOne (AddOne Zero) :: Nat
-- foo = Proxy

{-
>>> :t Proxy :: Proxy (AddOne (AddOne Zero))
Proxy :: Proxy (AddOne (AddOne Zero))

>>> :t Proxy :: Proxy (Comp AddOne AddOne Zero)
<interactive>:1:10: error:
    • The type family ‘AddOne’ should have 1 argument, but has been given none
    • In an expression type signature: Proxy (Comp AddOne AddOne Zero)
      In the expression: Proxy :: Proxy (Comp AddOne AddOne Zero)

>>> :t undefined :: Comp AddOne AddOne Zero
<interactive>:1:14: error:
    • Expected a type, but ‘Comp AddOne AddOne 'Zero’ has kind ‘Nat’
    • In an expression type signature: Comp AddOne AddOne Zero
      In the expression: undefined :: Comp AddOne AddOne Zero
-}