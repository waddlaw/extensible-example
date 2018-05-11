#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-11
   --package extensible
   --package text
   --package lens
-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Extensible

import           Control.Lens    (( # ), (^.))
import           Data.Proxy      (Proxy (Proxy))
import           Data.Text       (Text)

type ID = Text

data UserHs = UserHs
  { id   :: ID
  , name :: Text
  , age  :: Int
  }

type User = Record
  '[ "id"   >: ID
   , "name" >: Text
   , "age"  >: Int
   ]

user1 :: User
user1 = #id   @= "U123456789"
     <: #name @= "Alice"
     <: #age  @= 24
     <: nil

---

type User' = Record
  '[ "id"      >: ID
   , "name"    >: Text
   , "age"     >: Int
   , "address" >: Text
   ]

type A = Record '[ "foo" >: Text, "bar" >: Text ]
type B = Record '[ "bar" >: Text, "foo" >: Text ]

---

type User'' = Record
  '[ "address" >: Text
   , "id"      >: ID
   , "name"    >: Text
   , "age"     >: Int
   ]

user2 :: User''
user2 = #address @= "Japan"
     <: user1

user3 :: User'
user3 = shrink $ #address @= ("Japan" :: Text)
     <: user1

---

type User''' = Record
  '[ "name" >: Text
   , "age"  >: Int
   ]

---

type Color = Variant
  '[ "rgb"  >: (Int,Int,Int)
   , "cmyk" >: (Int,Int,Int,Int)
   ]

color1 :: Color
color1 = #rgb # (0, 0, 0)

color2 :: Color
color2 = #cmyk # (0, 0, 0, 0)

---

type RGB  = Variant '[ "rgb"  >: (Int,Int,Int) ]
type CMYK = Variant '[ "cmyk" >: (Int,Int,Int,Int) ]

color3 :: RGB
color3 = #rgb # (0, 0, 0)

color4 :: CMYK
color4 = #cmyk # (0, 0, 0, 0)

---

main :: IO ()
main = do
  print (user1 ^. #id)
  print (user1 ^. #age)
  print $ (Proxy :: Proxy A) == (Proxy :: Proxy A)
  print $ (Proxy :: Proxy B) == (Proxy :: Proxy B)
  print $ (Proxy :: Proxy A) == fmap shrink (Proxy :: Proxy B)
  print user2
  print user3
  print (shrink user1 :: User''')
  print color1
  print color2
  print (spread color3 :: Color)
  print (spread color4 :: Color)