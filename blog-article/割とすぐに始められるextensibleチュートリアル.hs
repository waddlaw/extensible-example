#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-12
   --package extensible
   --package lens
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import           Data.Extensible

import           Control.Lens    ((&), (.~), (^.))

mkField "name collective cry"

{-
*Main> :t name
name
  :: (ExtensibleConstr t (Field h) xs ("name" ':> v),
      Labelling "name" p, Extensible f p t, GHC.TypeNats.KnownNat n,
      Wrapper h,
      Elaborate "name" (FindAssoc 0 "name" xs) ~ 'Expecting (n ':> v)) =>
     Data.Extensible.Internal.Rig.Optic' p f (t (Field h) xs) (Repr h v)
-}

type Animal = Record
  '[ "name"       :> String
   , "collective" :> String
   , "cry"        :> Maybe String
   ]

dove :: Animal
dove = name       @= "dove"
    <: collective @= "dule"
    <: cry        @= Just "coo"
    <: nil

swan :: Animal
swan = name       @= "swan"
    <: collective @= "lamentation"
    <: cry        @= Nothing
    <: nil

-- collectiveOf :: Animal -> String
collectiveOf
  :: (Associate "name" String xs, Associate "collective" String xs)
  => Record xs -> String
collectiveOf a = unwords ["a", a ^. collective, "of", a ^. name ++ "s"]

main :: IO ()
main = do
  print dove
  print swan
  print $ swan ^. name
  print $ swan ^. #name
  print $ swan & collective .~ "bank"
  print $ swan & #collective .~ "bank"

  print $ collectiveOf dove
  print $ collectiveOf swan
