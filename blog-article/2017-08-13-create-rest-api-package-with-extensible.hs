#!/usr/bin/env stack
{- stack repl
   --resolver nightly-2018-05-11
   --package extensible
   --package text
   --package constraints
   --package req
   --package data-default
   --package lens
-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Data.Extensible
import           Data.Extensible.Dictionary (library)

import           Control.Lens               ((&), (.~))
import           Data.Functor.Identity      (Identity (..), runIdentity)
import           Data.Proxy                 (Proxy (Proxy))
import           GHC.TypeLits               (KnownSymbol, symbolVal)

import           Control.Applicative        (liftA2)
import           Data.Constraint            (Dict (..))
import           Data.Default               (Default, def)
import           Data.Monoid                (Endo (..))
import           Data.String                (fromString)
import           Data.Text                  (Text)
import           Network.HTTP.Req           (QueryParam, (=:))

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
type ItemWrap = Int
type GenreInformation = Int
type TagGroupWrap = Int

{- FromJSON は extensible クラスのインスタンスになっているので現在は定義せずに利用できる
instance Forall (KeyValue KnownSymbol FromJSON) xs => FromJSON (Record xs) where
  parseJSON = withObject "Object" $
    \v -> hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol FromJSON)) $
    \m -> let k = symbolVal (proxyAssocKey m) in
      case HM.lookup (fromString k) v of
        Just a -> Field . return <$> parseJSON a
        Nothing -> fail $ "Missing key: " `mappend` k
-}

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

instance Forall (KeyValue KnownSymbol ToParam) xs => ToParams (Record xs) where
  toParams = flip appEndo mempty . hfoldMap getConst' . hzipWith
    (\(Comp Dict) -> Const' . Endo . (<>) .
      liftA2 toParam (fromString . symbolVal . proxyAssocKey) getField)
    (library :: Comp Dict (KeyValue KnownSymbol ToParam) :* xs)

instance Default a => Default (Identity a) where
  def = Identity def

instance Default Text where
  def = mempty

instance Forall (KeyValue KnownSymbol Default) xs => Default (Record xs) where
  def = runIdentity $ hgenerateFor
    (Proxy :: Proxy (KeyValue KnownSymbol Default)) (const $ pure (Field def))

{- htabulateFor と TypeApplications を使うとコードが少しスッキリする
instance Forall (KeyValue KnownSymbol Default) xs => Default (Record xs) where
  def = htabulateFor poly (const . Field $ def)
    where
      poly = Proxy @ (KeyValue KnownSymbol Default)
-}

param :: IchibaItemSearchParam
param = def & #keyword .~ "Rakuten"
