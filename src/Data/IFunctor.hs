{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.IFunctor where

import Data.Data
import Data.Kind

type family Sing :: k -> Type

type SingI :: forall {k}. k -> Constraint
class SingI a where
  sing :: Sing a

infixr 0 ~>

type f ~> g = forall x. f x -> g x

class IFunctor f where
  imap :: (a ~> b) -> f a ~> f b

class (IFunctor m) => IMonad m where
  ireturn :: a ~> m a
  ibind :: (a ~> m b) -> m a ~> m b

class (IMonad m) => IMonadFail m where
  fail :: String -> m a ix

data At :: Type -> k -> k -> Type where
  At :: a -> At a k k
  deriving (Typeable)

(>>=) :: (IMonad (m :: (x -> Type) -> x -> Type)) => m a ix -> (a ~> m b) -> m b ix
m >>= f = ibind f m

(>>) :: (IMonad (m :: (x -> Type) -> x -> Type)) => m (At a j) i -> m b j -> m b i
m >> f = ibind (\(At _) -> f) m

returnAt :: (IMonad m) => a -> m (At a k) k
returnAt = ireturn . At
