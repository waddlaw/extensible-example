#!/usr/bin/env stack
{- stack repl
   --resolver lts-14.0
   --package extensible-0.6.1
   --package text
   --package constraints
   --package req
   --package data-default
   --package lens
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

import Data.Extensible

import Control.Lens ((&), (.~))
import Data.Functor.Identity (Identity (..), runIdentity)
import GHC.TypeLits (symbolVal)

import Control.Applicative (liftA2, Const(..))
import Data.Constraint (Dict (..))
import Data.Default (Default, def)
import Data.Monoid (Endo (..))
import Data.String (fromString)
import Data.Text (Text)
import Network.HTTP.Req (QueryParam, (=:))

type IchibaItems = Record
  '[ "count"            >: Int
   , "page"             >: Int
   , "first"            >: Int
   , "last"             >: Int
   , "hits"             >: Int
   , "carrier"          >: Int
   , "pageCount"        >: Int
   , "Items"            >: [ItemWrap]
   , "GenreInformation" >: [GenreInformation]
   , "TagInformation"   >: [TagGroupWrap]
   ]

-- 適当に定義
type ItemWrap         = Int
type GenreInformation = Int
type TagGroupWrap     = Int

type IchibaItemSearchParam = Record
  '[ "keyword"                 >: Text
   , "shopCode"                >: Maybe Text
   , "itemCode"                >: Maybe Text
   , "genreId"                 >: Maybe Int
   , "tagId"                   >: Maybe Int
   , "hits"                    >: Maybe Int
   , "page"                    >: Maybe Int
   , "sort"                    >: Maybe Text
   , "minPrice"                >: Maybe Int
   , "maxPrice"                >: Maybe Int
   , "availability"            >: Maybe Bool
   , "field"                   >: Maybe Bool
   , "carrier"                 >: Maybe Bool
   , "imageFlag"               >: Maybe Bool
   , "orFlag"                  >: Maybe Bool
   , "NGKeyword"               >: Maybe Text
   , "purchaseType"            >: Maybe Int
   , "shipOverseasFlag"        >: Maybe Bool
   , "shipOverseasArea"        >: Maybe Text
   , "asurakuFlag"             >: Maybe Bool
   , "asurakuArea"             >: Maybe Int
   , "pointRateFlag"           >: Maybe Bool
   , "pointRate"               >: Maybe Int
   , "postageFlag"             >: Maybe Bool
   , "creditCardFlag"          >: Maybe Bool
   , "giftFlag"                >: Maybe Bool
   , "hasReviewFlag"           >: Maybe Bool
   , "maxAffiliateRate"        >: Maybe Double
   , "minAffiliateRate"        >: Maybe Double
   , "hasMovieFlag"            >: Maybe Bool
   , "pamphletFlag"            >: Maybe Bool
   , "appointDeliveryDateFlag" >: Maybe Bool
   , "genreInformationFlag"    >: Maybe Bool
   , "tagInformationFlag"      >: Maybe Bool
   ]

class ToParam a where
  toParam :: (QueryParam param, Monoid param) => Text -> a -> param

instance ToParam Int where
  toParam = (=:)

instance ToParam a => ToParam (Maybe a) where
  toParam = maybe mempty . toParam

instance ToParam a => ToParam (Identity a) where
  toParam name = toParam name . runIdentity

class ToParams a where
  toParams :: (QueryParam param, Monoid param) => a -> param

instance Forall (KeyTargetAre KnownSymbol ToParam) xs => ToParams (Record xs) where
  toParams = flip appEndo mempty . hfoldMap getConst . hzipWith
    (\(Comp Dict) -> Const . Endo . (<>) .
      liftA2 toParam (fromString . symbolVal . proxyKeyOf) getField)
    (library :: xs :& Comp Dict (KeyTargetAre KnownSymbol ToParam))

instance Default a => Default (Identity a) where
  def = Identity def

instance Default Text where
  def = mempty

instance Forall (KeyTargetAre KnownSymbol Default) xs => Default (Record xs) where
  def = htabulateFor c (const . Field $ def)
    where
      c = Proxy @(KeyTargetAre KnownSymbol Default)

param :: IchibaItemSearchParam
param = def & #keyword .~ "Rakuten"
