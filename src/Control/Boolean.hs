{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Control.Boolean
-- Copyright   :  (c) The University of Kansas
-- License     :  BSD3
--
-- Maintainer  :  andygill@ku.edu
-- Stability   :  experimental
--
-- Some classes for generalized boolean operations, adapted for type functions.
--
-- In this design, for if-then-else, equality and inequality tests, the
-- boolean type depends on the value type.  A type function
-- allows the boolean type to be inferred in a conditional expression.
--
-- Based on the boolean package, (c) Conal Elliott 2009.
--
----------------------------------------------------------------------

module Control.Boolean
  ( Boolean(..)
  , BooleanOf
  , IfB(..), boolean, cond, crop
  , EqB(..), OrdB(..), minB, maxB
  ) where

import Data.Monoid (Monoid,mempty)
import Control.Applicative (Applicative(pure),liftA2,liftA3)

{--------------------------------------------------------------------
    Classes
--------------------------------------------------------------------}

infixr 3  &&*
infixr 2  ||*

-- | Generalized boolean class
class Boolean b where
  true, false  :: b
  notB         :: b -> b
  (&&*), (||*) :: b -> b -> b

instance Boolean Bool where
  true  = True
  false = False
  notB  = not
  (&&*) = (&&)
  (||*) = (||)

-- | 'BooleanOf' computed the boolean analog of a specific type.
type family BooleanOf a

-- | Types with conditionals
class (Boolean (BooleanOf a)) => IfB a where
  ifB  :: (bool ~ BooleanOf a) => bool -> a -> a -> a

-- | Expression-lifted conditional with condition last
boolean :: IfB a => a -> a -> BooleanOf a -> a
boolean t e bool = ifB bool t e

-- | Point-wise conditional
cond :: (Applicative f, IfB a, bool ~ BooleanOf a) => f bool -> f a -> f a -> f a
cond = liftA3 ifB

-- | Crop a function, filling in 'mempty' where the test yeis false.
crop :: (Applicative f, Monoid (f a), IfB a, bool ~ BooleanOf a) => f bool -> f a -> f a
crop r f = cond r f mempty

infix  4  ==*, /=*

-- | Types with equality.  Minimum definition: '(==*)'.
class (Boolean (BooleanOf a)) => EqB a where
  (==*), (/=*) :: (bool ~ BooleanOf a) => a -> a -> bool
  u /=* v = notB (u ==* v)

infix  4  <*, <=*, >=*, >*

-- | Types with inequality.  Minimum definition: '(<*)'.
class (Boolean (BooleanOf a)) => OrdB a where
  (<*), (<=*), (>*), (>=*) :: (bool ~ BooleanOf a) => a -> a -> bool
  u >*  v = v <* u
  u >=* v = notB (u <* v)
  u <=* v = v >=* u

-- | Variant of 'min' using 'ifB' and '(<=*)'
minB :: (IfB a, OrdB a) => a -> a -> a
u `minB` v = ifB (u <=* v) u v

-- | Variant of 'max' using 'ifB' and '(>=*)'
maxB :: (IfB a, OrdB a) => a -> a -> a
u `maxB` v = ifB (u >=* v) u v


-- Instances for Prelude types.

type instance BooleanOf Int     = Bool
type instance BooleanOf Integer = Bool
type instance BooleanOf Float   = Bool
type instance BooleanOf Double  = Bool
type instance BooleanOf Bool    = Bool
type instance BooleanOf Char    = Bool
type instance BooleanOf [a]     = BooleanOf a

instance IfB Int where
  { ifB i t e = if i then t else e }
instance IfB Integer where
  { ifB i t e = if i then t else e }
instance IfB Float where
  { ifB i t e = if i then t else e }
instance IfB Double where
  { ifB i t e = if i then t else e }
instance IfB Bool where
  { ifB i t e = if i then t else e }
instance IfB Char where
  { ifB i t e = if i then t else e }

instance EqB  Int where
  { (==*) = (==); (/=*) = (/=) }
instance OrdB Int where
  { (<*) = (<) }
instance EqB  Integer where
  { (==*) = (==); (/=*) = (/=) }
instance OrdB Integer where
  { (<*) = (<) }
instance EqB  Float where
  { (==*) = (==); (/=*) = (/=) }
instance OrdB Float where
  { (<*) = (<) }
instance EqB  Double where
  { (==*) = (==); (/=*) = (/=) }
instance OrdB Double where
  { (<*) = (<) }
instance EqB  Bool where
  { (==*) = (==); (/=*) = (/=) }
instance OrdB Bool where
  { (<*) = (<) }
instance EqB  Char where
  { (==*) = (==); (/=*) = (/=) }
instance OrdB Char where
  { (<*) = (<) }

-- Instances for functions, using the standard pattern for applicative functions.

instance Boolean bool => Boolean (z -> bool) where
  true  = pure true
  false = pure false
  notB  = fmap notB
  (&&*) = liftA2 (&&*)
  (||*) = liftA2 (||*)

type instance BooleanOf (z -> a) = z -> BooleanOf a

instance IfB a => IfB (z -> a) where
  ifB = cond

instance EqB a => EqB  (z -> a) where
  { (==*) = liftA2 (==*) ; (/=*) = liftA2 (/=*) }
instance OrdB a => OrdB (z -> a) where
  { (<*) = liftA2(<*) ; (<=*) = liftA2(<=*) }

