{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import Data.Extensible
import Data.Extensible.GetOpt
import Data.Proxy
import Control.Lens hiding ((:>))
import Data.Typeable

main :: IO ()
main = do
  print $ baz ^. #str
  print $ typeOf (test2 ^. #num)

type A =
  Record '[
      "name" ':> String
    , "age" ':> Int
  ]

type B =
  Record '[
      "name" ':> String
    , "age" ':> Int
    , "isHuman" ':> Bool
  ]

person1 :: A
person1 = #name @= "Guchi"
  <: #age @= 18
  <: emptyRecord

person2 :: B
person2 = #name @= "guchi"
  <: #age @= 18
  <: #isHuman @= True
  <: emptyRecord

person3 :: B
person3 = shrink $
  #isHuman @= False
  <: person1

foo :: Record '["num" >: Int, "str" >: String]
foo = #num @= 42
  <: #str @= "foo"
  <: nil

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

-- |
-- >>> print baz
-- num @= Nothing <: str @= Just "foo" <: nil
test :: RecordOf OptDescr' '["num" >: Int, "str" >: [String]]
test = #num @= optNoArg "v" ["verbose"] "verbose"
  <: #str @= optReqArg "e" ["extra"] "ARG" "extra arguments"
  <: nil

test2 :: RecordOf Proxy '["num" >: MyData, "str" >: MyData]
test2 = #num @= ()
  <: #str @= ()
  <: nil

data MyData = MyDataA | MyDataB

-- AssocValue (Proxy :: "num" >: MyData)