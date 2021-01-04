{-# LANGUAGE UndecidableSuperClasses #-}
module Utils where

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(..))

-- Type level utilities
type (f :: i -> o) $ (x :: i) = f x
infixr 0 $

type (x :: k) := (f :: k -> Constraint) = f x
infix 0 :=

data HList (types :: [Type])
  where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

data Location (xs :: [k]) (x :: k)
  where
  Here :: Location (x ': xs) x
  There :: Location xs x -> Location (v ': xs) x

class HIndex (items :: HList xs) (i :: Location xs k) (r :: k) | items i -> r

instance HIndex ('HCons x xs) 'Here x
instance HIndex xs l r => HIndex ('HCons x xs) ('There l) r

type MappedHList :: (a -> b) -> [a] -> [b] -> *
data MappedHList f as bs
  where
  MHNil :: MappedHList f xs xs
  MHCons :: f x -> MappedHList f xs ys -> MappedHList f (x ': xs) (f x ': ys)

data Optional enabled content
  where
  Absent :: Optional 'False content
  Present :: content -> Optional 'True content

-- Class manipulation utilities

-- Classes that are functionally dependent
type FDep :: (a -> b -> Constraint) -> a -> b -> Constraint
class c x y => FDep c x y | c x -> y

-- Term level function for evaluating a type level function on a type level argument
(^$) :: FDep c x y => Proxy c -> Proxy x -> Proxy y
(^$) _ _ = Proxy

infixr 0 ^$

-- Constraint-enriched endofunctors
type CMapping f = forall x y. (x -> y -> Constraint) -> f x -> f y -> Constraint
class CFunctor (f :: Type -> Type)
  where
  type CMap :: CMapping f

-- Lists
type CMapList :: CMapping []
class CMapList c xs ys | c xs -> ys
instance CMapList c xs ys => FDep (CMapList c) xs ys

instance CMapList (FDep c) '[] '[]
instance (FDep c x y, CMapList (FDep c) xs ys) => CMapList (FDep c) (x ': xs) (y ': ys)

instance CFunctor []
  where
  type CMap = CMapList

-- Tuples
type CMapTuple :: CMapping ((,) a)
class CMapTuple c ax ay | c ax -> ay
instance CMapTuple c ax ay => FDep (CMapTuple c) ax ay

instance FDep c x y => CMapTuple (FDep c) '(a, x) '(a, y)

instance CFunctor ((,) a)
  where
  type CMap = CMapTuple

-- TODO: Revisit functor composition

-- Tuple stuff
class Fst ab a | ab -> a
instance Fst '(a, b) a
instance FDep Fst '(a, b) a

class Snd ab b | ab -> b
instance Snd '(a, b) b
instance FDep Snd '(a, b) b
