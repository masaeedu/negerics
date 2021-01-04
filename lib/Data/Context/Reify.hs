module Data.Context.Reify where

import Utils
import Data.Context
import Data.Description
import GHC.Exts

-- Now we can interpret a data expression into a runtime representation in any suitable data context
type Reify :: forall k pkinds datatype. HList pkinds -> datatype pkinds k -> (k -> k) -> Constraint
class Reify args description representation | args description -> representation
instance Reify args description representation => FDep (Reify args) description representation

-- Reification of datatypes
instance Reify args c r => Reify args ('Newtype 'Absent c) r
instance Cocartesian k => Reify @k args ('Data 'Absent '[]) Init
instance (Semicocartesian k, Reify args c r, Reify args ('Data 'Absent cs) r') => Reify @k args ('Data 'Absent (c ': cs)) (r :+: r')

-- Reification of constructors
instance Reify args fl r => Reify args ('Constructor 'Absent name fl) r

-- Reification of field lists
instance Cartesian k => Reify @k args ('Tuple '[]) Term
instance (Semicartesian k, Reify args f r, Reify args ('Tuple fs) r') => Reify @k args ('Tuple (f ': fs)) (r :*: r')

instance Cartesian k => Reify @k args ('Record '[]) Term
instance (Semicartesian k, Reify args f r, Reify args ('Record fs) r') => Reify @k args ('Record ('(s, f) ': fs)) (r :*: r')

-- Reification of fields
instance Reify args expr r => Reify args ('Field 'Absent expr) r

-- Reification of field expressions
instance Reify args ('Refer t) (K t)
instance (Reify args f f', Reify args x x') => Reify args ('Apply f x) (f' x')
instance HIndex args i r => Reify args ('Param i) (K r)
instance Reify args 'RgRec I
