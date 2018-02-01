# Data.Extensible.Field

## (@=)

`(@=) :: Wrapper h => FieldName k -> Repr h v -> Field h (k :> v) `

```haskell
-- |
-- >>> print foo
-- num @= 42 <: str @= "foo" <: nil
foo :: Record '["num" >: Int, "str" >: String]
foo = #num @= 42
  <: #str @= "foo"
  <: nil
```

型を展開する。

```haskell
foo :: Record '["num" >: Int, "str" >: String]
    :: RecordOf Identity '["num" >: Int, "str" >: String]
    :: (:*) (Field Identity) '["num" >: Int, "str" >: String]
    :: Field Identity :* '["num" >: Int, "str" >: String]

type Record = RecordOf Identity
type RecordOf h = (:*) (Field h)
```

さらに進む

```haskell
foo :: Record '["num" >: Int, "str" >: String]
    :: RecordOf Identity '["num" >: Int, "str" >: String]
    :: (:*) (Field Identity) '["num" >: Int, "str" >: String]
    :: Field Identity :* '["num" >: Int, "str" >: String]
    :: Field Identity :* '["num" :> Int, "str" :> String]

type (>:) = (:>)
```

ここでそれぞれの `kind` を考える。

```haskell
"num" :> Int    :: Assoc Symbol *
"str" :> String :: Assoc Symbol *

'["num" :> Int, "str" :> String] :: [Assoc Symbol *]

Identity       :: * -> *
Field Identity :: Assoc k * -> *

(Field Identity :*) :: [Assoc k *] -> *
Field Identity :* '["num" :> Int, "str" :> String] :: *


data h :* s where
  (:*) :: (k -> *) -> [k] -> *

data Assoc k v where
  k :> v

newtype Field (h :: v -> *) (kv :: Assoc k v) = Field { getField :: h (AssocValue kv) }
```

- `Symbol` は [GHC.Types](https://www.stackage.org/haddock/lts-9.14/ghc-prim-0.5.0.0/GHC-Types.html#t:Symbol) で定義されている。

`Wrapper` を色々と変えてみた

```haskell
-- |
-- >>> print bar
-- num @= [] <: str @= ["foo","12"] <: nil
bar :: RecordOf Maybe '["num" >: Int, "str" >: String]
bar = #num @= Nothing
  <: #str @= Just "foo"
  <: nil

-- |
-- >>> print baz
-- num @= Nothing <: str @= Just "foo" <: nil
baz :: RecordOf [] '["num" >: Int, "str" >: String]
baz = #num @= []
  <: #str @= ["foo", "12"]
  <: nil
```

Proxy の場合

```haskell
(@=) :: Wrapper h => FieldName k -> Repr h v -> Field h (k :> v)
     :: FieldName k -> Repr Proxy v -> Field Proxy (k :> v)
     :: FieldName k -> () -> Field Proxy (k :> v)

RecordOf Proxy '["num" >: "abc", "str" >: "abc"]
(:*) (Field Proxy) ["num" >: "abc", "str" >: "abc"]

Proxy :: k -> *
Field Proxy :: Assoc k1 k2 -> *
```

動いた

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Data.Extensible
import Data.Proxy
import Data.Typeable

data MyData = MyDataA | MyDataB

foo :: Proxy "abc"
foo = (Proxy :: Proxy (AssocValue ("num" >: "abc")))
```

実行結果

```shell
>>> typeRep foo
MyData
```