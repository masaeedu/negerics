{-# LANGUAGE DeriveGeneric, PartialTypeSignatures #-}
module Example where

import Data.Proxy (Proxy(..))
import GHC.Generics (Generic, Generic1)
import Utils (Location(..), HList(..), type ($), (^$), Optional(..))
import Data.Description (LField, FieldExpression(..), FieldList(..), Field(..), Constructor(..), ConstructorInfo(..), DataType(..), DataTypeInfo(..))
import Data.Context.Reify

-- ..., we can mechanically derive these for various arities

-- You can do this:
data Tree0 a = Leaf0 a | Tree0 a :^: Tree0 a
  deriving Generic

data Tree1 a x = Leaf1 (a x) | Tree1 a x :^^: Tree1 a x
  deriving (Generic, Generic1)

data Tree2 a x y = Leaf2 (a x y) | Tree2 a x y :^^^: Tree2 a x y deriving (Generic, Generic1 {-, Generic2 -})

-- ...

-- Or you can do this:
type TreeNRep :: forall k. DataType 'True '[k] k
type TreeNRep = 'Data ('Present $ 'DataTypeInfo "negerics" "Lib" "TreeN")
  [ 'Constructor ('Present 'Prefix)                    "Leaf" $ 'Tuple '[ LField $ 'Param $ 'Here ]
  , 'Constructor ('Present $ 'Infix 'Nothing 'Nothing) ":^:"  $ 'Tuple
      [ LField $ 'RgRec
      , LField $ 'RgRec
      ]
  ]

type TreeNRep' :: forall k. DataType 'False '[k] k
type TreeNRep' = 'Data 'Absent
  [ 'Constructor 'Absent "Leaf" $ 'Tuple '[ 'Field 'Absent $ 'Param $ 'Here ]
  , 'Constructor 'Absent ":^:"  $ 'Tuple
      [ 'Field 'Absent $ 'RgRec
      , 'Field 'Absent $ 'RgRec
      ]
  ]

test :: Proxy _
test = Proxy @(Reify $ 'HCons [] $ 'HNil) ^$ Proxy @TreeNRep'
