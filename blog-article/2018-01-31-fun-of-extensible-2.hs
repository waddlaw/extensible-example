#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-19
   --package extensible-0.4.9
   --package lens
-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Data.Extensible

import Control.Lens          (( # ), (^.))
import Data.Functor.Identity (runIdentity)
import Data.Proxy            (Proxy (Proxy))
import GHC.TypeLits          (KnownSymbol, symbolVal)

{-
data Color
  = RGB  Int Int Int
  | CMYK Int Int Int Int
-}

type Color = Variant
  '[ "rgb"  >: (Int, Int, Int)
   , "cmyk" >: (Int, Int, Int, Int)
   ]

type RGB = Variant '[ "rgb" >: (Int, Int, Int) ]

color1 :: RGB
color1 = #rgb # (0, 0, 0)

color2 :: Color
color2 = spread color1

---

{-
data Shape
  = Circle Point Double
  | Rect   Point Point

type Point = (Double, Double)

area :: Shape -> Double
area (Circle _ r)           = pi * (r ^ 2)
area (Rect (x1,y1) (x2,y2)) = abs (x2 - x1) * abs (y2 - y1)
-}

type Shape = Variant
  '[ "circle" >: Circle
   , "rect"   >: Rect
   ]

type Point = Record
  '[ "x" >: Double
   , "y" >: Double
   ]

-- newtype Circle = Circle (Record '[ "mid" >: Point, "r" >: Double ]) deriving (Show, Eq)
-- newtype Rect   = Rect   (Record '[ "ll" >: Point, "ur" >: Point ])  deriving (Show, Eq)

type Circle = Record
  '[ "p" >: Point
   , "r" >: Double
   ]
type Rect = Record
  '[ "leftTop"     >: Point
   , "rightBottom" >: Point
   ]

--

area :: Shape -> Double
area = matchField
     $ #circle @= (\circle -> pi * (circle ^. #r) ^ 2)
    <: #rect   @= ((*) <$> width <*> height)
    <: nil

width, height :: Rect -> Double
width  rect = abs $ rect ^. #rightBottom ^. #x - rect ^. #leftTop ^. #x
height rect = abs $ rect ^. #rightBottom ^. #y - rect ^. #leftTop ^. #y

---

class Area a where
  area' :: a -> Double

instance Area Circle where
  area' circle = pi * (circle ^. #r) ^ 2

instance Area Rect where
  area' = (*) <$> width <*> height

instance Forall (KeyValue KnownSymbol Area) xs => Area (Variant xs) where
  area' = matchField $
    htabulateFor c (const . Field . pureMatch $ area')
    where c = Proxy @ (KeyValue KnownSymbol Area)

pureMatch :: (x -> r) -> Match Identity r x
pureMatch f = Match $ f . runIdentity

---

newtype Triangle = Triangle (Point, Point, Point) deriving (Show, Eq)

type Shape2 = Variant
  '[ "circle"   >: Circle
   , "rect"     >: Rect
   , "triangle" >: Triangle
   ]

instance Area Triangle where
  area' (Triangle (p1, p2, p3)) =
    abs ((p1 ^. #x - p3 ^. #x) * (p2 ^. #y - p3 ^. #y) - (p2 ^. #x - p3 ^. #x) * (p1 ^. #y - p3 ^. #y)) / 2

---

main :: IO ()
main = do
  print color1
  print color2

  let
    shape1 = #circle # circle                            :: Shape
    shape2 = #rect # rect                                :: Shape
    circle = #p @= p1 <: #r @= 2.0 <: nil                :: Circle
    rect   = #leftTop @= p1 <: #rightBottom @= p2 <: nil :: Rect
    p1     = #x @= 10.0 <: #y @= 10.0 <: nil             :: Point
    p2     = #x @= 200.0 <: #y @= 100.0 <: nil           :: Point

  print $ area shape1
  print $ area shape2
  print $ width rect
  print $ height rect

  print $ area' shape1
  print $ area' shape2
  print $ area' rect

  let
    shape3 = #triangle # triangle      :: Shape2
    triangle = Triangle (p4, p5, p6)   :: Triangle
    p4 = #x @= 0 <: #y @= 0 <: nil     :: Point
    p5 = #x @= 100.0 <: #y @= 0 <: nil :: Point
    p6 = #x @= 0 <: #y @= 100 <: nil   :: Point

  print $ area' triangle
  print $ area' shape3
