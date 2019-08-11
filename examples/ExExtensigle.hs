{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Extensible
import Data.Text (Text)
import Data.Kind

type A = Record '[
  "name" >: Text
  ]

type B = Record '[
  "age" >: Int
  ]

type C = AppendRecord A B

type family AppendRecord (r1 :: *) (r2 :: *) :: * where
  AppendRecord (h:*xs) (h2:*ys) = h:*(xs++ys)

p1 :: A
p1 = #name @= "guchi"
  <: emptyRecord

p2 :: B
p2 = #age @= 18
  <: emptyRecord

p3 :: C
p3 = #name @= "guchi"
  <: #age @= 18
  <: emptyRecord

main :: IO ()
main = do
  print p1
  print p2
  print $ p1 `happend` p2
  print $ p3