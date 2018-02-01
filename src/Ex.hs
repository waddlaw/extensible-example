{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

import Data.Extensible
import Data.Text
import Data.Proxy
import GHC.TypeLits

newtype AssocValue' a = AssocValue' { getValue :: Proxy (AssocValue a) }

baz :: RecordOf Proxy '["name" >: "guchi", "age" >: "18"]
baz = #name @= () <: #age @= () <: nil

proxy :: Proxy (KeyValue KnownSymbol KnownSymbol)
proxy = Proxy

-- hfoldMapFor proxy ((: []) . symbolVal . getValue) $  hmap (AssocValue' . getField) baz
-- hfoldMapFor proxy ((: []) . symbolVal . getField) baz