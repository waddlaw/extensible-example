{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
import Data.Proxy
import GHC.TypeLits

class ManySymbolVal (xs :: [Symbol]) where
  manySymbolVal :: proxy xs -> [String]

instance ManySymbolVal '[] where
  manySymbolVal _ = []

instance (KnownSymbol a, ManySymbolVal as) => ManySymbolVal (a ': as) where
  manySymbolVal _ =
    symbolVal (Proxy :: Proxy a) : manySymbolVal (Proxy :: Proxy as)

myProxy :: Proxy '["hello","small","world"]
myProxy = Proxy

main :: IO ()
main = mapM_ putStrLn (manySymbolVal myProxy)