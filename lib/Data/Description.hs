module Data.Description where

import GHC.TypeLits (Symbol)
import Utils (Optional(..), FDep, CFunctor(..), Location(..))

-- The language of @data@ declarations

-- Datatypes
data DataTypeInfo = DataTypeInfo
  { packageName :: Symbol
  , moduleName :: Symbol
  , dataTypeName :: Symbol
  }
data DataType m pkinds kind
  = Newtype (Optional m DataTypeInfo) (Constructor m pkinds kind)
  | Data    (Optional m DataTypeInfo) [Constructor m pkinds kind]

-- Constructors
data Fixity
  = F0 | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9
data Associativity = LeftAssociative | RightAssociative
data ConstructorInfo
  = Infix (Maybe Fixity) (Maybe Associativity)
  | Prefix
data Constructor m pkinds kind = Constructor
  { constructorInfo :: Optional m ConstructorInfo
  , constructorName :: Symbol
  , fields :: FieldList m pkinds kind
  }

data FieldList m pkinds kind
  = Record [(Symbol, Field m pkinds kind)]
  | Tuple [Field m pkinds kind]

data DecidedStrictness = DecidedLazy | DecidedStrict | DecidedUnpack
data FieldInfo = FieldInfo
  { sourceUnpackedness :: Maybe Bool
  , sourceStrictness :: Maybe Bool
  , decidedStrictness :: DecidedStrictness
  }
data Field m pkinds kind = Field
  { fieldInfo :: Optional m FieldInfo
  , fieldExpression :: FieldExpression pkinds kind
  }

data FieldExpression pkinds k
  where
  Refer :: k -> FieldExpression pkinds k
  Apply :: FieldExpression pkinds (i -> o) -> FieldExpression pkinds i -> FieldExpression pkinds o
  Param :: Location pkinds k -> FieldExpression pkinds k
  RgRec :: FieldExpression pkinds k
  -- Recur :: MappedHList (FieldExpression pkinds) pkinds fieldexps -> FieldExpression pkinds k

-- Convenient type aliases
type DefaultStrictnessInfo :: FieldInfo
type DefaultStrictnessInfo = 'FieldInfo 'Nothing 'Nothing 'DecidedLazy

type LField = 'Field ('Present DefaultStrictnessInfo)

-- Strip metadata from representation
class StripMeta (meta :: description 'True params r) (nometa :: description 'False params r) | meta -> nometa
instance StripMeta meta nometa => FDep StripMeta meta nometa

-- Strip metadata from datatypes
instance StripMeta c c' => StripMeta ('Newtype ('Present i) c) ('Newtype 'Absent c')
instance FDep (CMap @[] (FDep StripMeta)) cs cs' => StripMeta ('Data ('Present i) cs) ('Data 'Absent cs')

-- Strip metadata from constructors
instance StripMeta fs fs' => StripMeta ('Constructor ('Present i) n fs) ('Constructor 'Absent n fs')

-- Strip metadata from field lists
instance FDep (CMap (FDep (CMap (FDep StripMeta)))) fs fs' => StripMeta ('Record fs) ('Record fs')
instance FDep (CMap (FDep StripMeta)) fs fs' => StripMeta ('Tuple fs) ('Tuple fs')

-- Strip metadata from fields
instance StripMeta ('Field ('Present x) e) ('Field 'Absent e)
