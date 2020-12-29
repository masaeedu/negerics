{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE EmptyDataDecls, DeriveGeneric, TypeFamilies, FunctionalDependencies, StandaloneKindSignatures #-}
module Lib where

import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import GHC.Exts (Constraint)
import GHC.TypeLits (Symbol)
import Data.Kind (Type)

type (f :: i -> o) $ (x :: i) = f x
infixr 0 $

type (x :: k) := (f :: k -> Constraint) = f x
infix 0 :=

data HList (types :: [Type])
  where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

data Nat = Z | S Nat

data Fin (i :: Nat)
  where
  FZ :: Fin ('S 'Z)
  FS :: Fin n -> Fin ('S n)

data Location (xs :: [k])
  where
  Here :: Location (x ': xs)
  There :: Location xs -> Location (x ': xs)

type (!) :: forall x. forall (xs :: [x]) -> Location xs -> x
type family xs ! l
  where
  (x ': xs) ! 'Here    = x
  (x ': xs) ! 'There l = xs ! l

type (<$>) :: forall a b. (a -> b) -> [a] -> [b]
type family f <$> xs
  where
  _ <$> '[] = '[]
  f <$> (x ': xs) = f x ': f <$> xs

data DatatypeInfo = DatatypeInfo
  { packageName :: Symbol
  , moduleName :: Symbol
  , dataTypeName :: Symbol
  }

data Datatype pkinds
  = Newtype DatatypeInfo (Constructor pkinds)
  | Data    DatatypeInfo [Constructor pkinds]

data Fixity
  = Fix0 | Fix1 | Fix2 | Fix3 | Fix4 | Fix5 | Fix6 | Fix7 | Fix8 | Fix9
data Associativity = LeftAssociative | RightAssociative
data ConstructorInfo
  = Infix Symbol (Maybe Fixity) (Maybe Associativity)
  | Prefix Symbol
data Constructor pkinds = Constructor
  { constructorInfo :: ConstructorInfo
  , fields :: FieldList pkinds
  }

data DecidedStrictness = DecidedLazy | DecidedStrict | DecidedUnpack
data StrictnessInfo = StrictnessInfo
  { sourceUnpackedness :: Maybe Bool
  , sourceStrictness :: Maybe Bool
  , decidedStrictness :: DecidedStrictness
  }
data FieldList pkinds
  = Record [(Symbol, Field pkinds)]
  | Tuple [Field pkinds]
data Field pkinds = Field
  { fieldStrictnessInfo :: StrictnessInfo
  , fieldExpression :: Expression pkinds Type
  }

data Expression (pkinds :: [Type]) (k :: Type)
  where
  Refer :: k -> Expression pkinds k
  Apply :: Expression pkinds (i -> o) -> Expression pkinds i -> Expression pkinds o
  Recur :: HList (Expression pkinds <$> pkinds) -> Expression pkinds Type
  Param :: Proxy i -> Expression pkinds (pkinds ! i)

type DefaultStrictnessInfo :: StrictnessInfo
type DefaultStrictnessInfo = 'StrictnessInfo 'Nothing 'Nothing 'DecidedLazy

type LField = 'Field DefaultStrictnessInfo

data Tree a = Leaf a | Tree a :^: Tree a
  deriving Generic

type TreeRep :: Datatype '[Type]
type TreeRep = 'Data ('DatatypeInfo "negerics" "Lib" "Tree")
  [ 'Constructor ('Prefix "Leaf") $ 'Tuple '[ LField $ 'Param $ 'Proxy @'Here ]
  , 'Constructor ('Infix ":^:" 'Nothing 'Nothing) $ 'Tuple
      [ LField $ 'Recur $ ('Param ('Proxy @'Here) '`HCons` 'HNil)
      , LField $ 'Recur $ ('Param ('Proxy @'Here) '`HCons` 'HNil)
      ]
  ]

data Foo = Foo (Tree Int)
  deriving Generic

data Void
  deriving Generic

message :: String
message = "Ready? Get set... GO!"
