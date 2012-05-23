{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Boolean
-- Copyright   :  (c) Conal Elliott 2009, (c) The University of Kansas
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net, andygill@ku.edu
-- Stability   :  experimental
--
-- Some classes for generalized boolean operations, adapted for type functions,
--
-- In this design, for if-then-else, equality and inequality tests, the
-- boolean type depends on the value type.  This type function
-- allows the boolean type to be inferred in a conditional expression.
--
----------------------------------------------------------------------

module Data.Boolean
  ( Boolean(..)
  , BooleanOf
  , IfB(..), boolean, cond, crop
  , EqB(..), OrdB(..), minB, maxB
  ) where

import Data.Monoid (Monoid,mempty)
import Control.Applicative (Applicative(pure,(<*>)),liftA2,liftA3)

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

{--------------------------------------------------------------------
    Some instances
--------------------------------------------------------------------}

ife :: Bool -> a -> a -> a
ife c t e = if c then t else e

data Id a = Id a
-- Monad, Functor, Applicative Functor

instance Applicative Id where
        pure = Id
        Id f <*> Id a = Id (f a)

instance Functor Id where
        fmap f (Id a) = Id (f a)

type instance BooleanOf (Id a) = Id Bool

instance Boolean (Id Bool) where
  true = pure true
  false = pure false
  notB  = fmap notB
  (&&*) = liftA2 (&&*)
  (||*) = liftA2 (||*)

instance IfB (Id a) where ifB (Id b) = ife b
instance (Eq a) => EqB (Id a) where { (==*) = liftA2 (==) ; (/=*) = liftA2 (/=) }
instance (Ord a) => OrdB (Id a) where { (<*) = liftA2 (<) ; (<=*) = liftA2 (<=) }

-- Standard pattern for applicative functors:

instance Boolean bool => Boolean (z -> bool) where
  true  = pure true
  false = pure false
  notB  = fmap notB
  (&&*) = liftA2 (&&*)
  (||*) = liftA2 (||*)


type instance BooleanOf (a -> b) = a -> BooleanOf b
instance IfB a => IfB (z -> a) where
  ifB = cond

instance EqB a => EqB  (z -> a) where
  { (==*) = liftA2 (==*) ; (/=*) = liftA2 (/=*) }
instance OrdB a => OrdB (z -> a) where
  { (<*) = liftA2(<*) ; (<=*) = liftA2(<=*) }

