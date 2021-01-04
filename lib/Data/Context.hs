module Data.Context where

import Data.Kind (Type)

-- Contexts in which we can interpret the language
class Semicocartesian k
  where
  data (:+:) :: k -> k -> k
  infixr 5 :+:

class Semicocartesian k => Cocartesian k
  where
  data Init :: k

class Semicartesian k
  where
  data (:*:) :: k -> k -> k
  infixr 6 :*:

class Cartesian k
  where
  data Term :: k

class DataContext k
  where
  data I :: k -> k
  data K :: k -> (k -> k)
  data Mu :: (k -> k) -> k

-- {{{ Instances for *

instance Semicocartesian Type
  where
  data a :+: b = L0 a | R0 b

instance Cocartesian Type
  where
  data Init

instance Semicartesian Type
  where
  data a :*: b = Pair0 { fst0 :: a, snd0 :: b }

instance Cartesian Type
  where
  data Term = Term0

instance DataContext Type
  where
  data I x = I0 { getI0 :: x }
  data K x y = K0 { getK0 :: x }
  data Mu f = Mu0 { runMu0 :: f (Mu f) }

-- }}}

-- {{{ Instances for (* -> *)

newtype f ~> g = Nat { runNat :: forall x. f x -> g x }

instance Semicocartesian (Type -> Type)
  where
  data (a :+: b) x = L1 (a x) | R1 (b x)

instance Cocartesian (Type -> Type)
  where
  data Init x

instance Semicartesian (Type -> Type)
  where
  data (a :*: b) x = Pair1 { fst1 :: a x, snd1 :: b x }

instance Cartesian (Type -> Type)
  where
  data Term x = Term1 x

instance DataContext (Type -> Type)
  where
  data I f x = I1 { getI1 :: f x }
  data K f g x = K1 { getK1 :: f x }
  data Mu f x = Mu1 { runMu1 :: f (Mu f) x }

-- }}}
