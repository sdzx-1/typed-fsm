{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.SR where

import Data.Data
import Data.Kind

type family Sing :: k -> Type

type SingI :: forall {k}. k -> Constraint
class SingI a where
  sing :: Sing a

class Reify (a :: ps) where
  reifyProxy :: Proxy a -> ps

reify :: forall ps (a :: ps). (Reify a) => ps
reify = reifyProxy (Proxy :: Proxy a)