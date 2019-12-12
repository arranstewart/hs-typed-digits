
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|

Module      : Data.TypedDigits.Internal
Description : Data.TypedDigits internals
Portability : portable

This module is considered internal, and may change
without warning.
The Package Versioning Policy does not apply to it.

= Description

Digits, indexed by their base at the type level.

-}

module Data.TypedDigits.Internal (
    module Data.TypedDigits.Internal
  , KnownNat 
  )
  where

import Data.Maybe
import Data.Proxy
import Data.Singletons.TypeLits (Nat, Sing(..), KnownNat )
import Data.Singletons          (singByProxy, SingI(..), fromSing )

-- | digits, indexed by their base at the type level
newtype Digit (base :: Nat) = Digit { getVal :: Int }

-- | @getBaseT p@: if @p@ is of some type @a base@,
-- then reflect the base into a value.
--
-- Useful for getting the base, just given a 'Proxy'; e.g.:
--
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> let p = Proxy :: Proxy 3
-- >>> getBaseT p
-- 3
getBaseT :: forall a base . (KnownNat base) => a base -> Int
getBaseT _ = fromIntegral $ fromSing s
  where
      s :: Sing base
      s = singByProxy (Proxy :: Proxy base)


-- | @digit x@: construct a digit. 0 <= @x@ < base must hold
-- true.
--
-- Nicer if TypeApplications is enabled, then you can say:
--
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> digit @9 8
-- Just 8 (base 9)
digit :: forall base . (KnownNat base) => Int -> Maybe (Digit base)
digit x = 
    if x >= 0 && x < fromIntegral (getBaseT (Proxy :: Proxy base))
    then Just $ Digit x
    else Nothing

-- | Like 'digit', but throws an 'ErrorCall' if the value is
-- out of range.
-- 
-- sample usage:
-- 
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> digit' @9 7
-- 7 (base 9)
digit' :: KnownNat base => Int -> Digit base
digit' x = fromMaybe (error "bad value for base") (digit x)

-- get the Sing type fore the base, given some digit
digitSing :: KnownNat base => Digit base -> Sing base
digitSing Digit{} =
    sing

-- | @getBase d@: return the base of the digit @d@
getBase :: KnownNat base => Digit base -> Int
getBase d = fromIntegral $ fromSing (digitSing d)

deriving instance Eq (Digit base)
deriving instance Ord (Digit base)

instance (KnownNat base) => Show (Digit base) where
  show d = show (getVal d) ++ " (base " ++ show (getBase d) ++ ")"

instance KnownNat base => Enum (Digit base) where
  toEnum = digit'
  fromEnum = getVal

instance (KnownNat base) => Bounded (Digit base) where
  minBound = digit' 0

  maxBound = digit' $ n - 1
    where
      n = getBaseT (Proxy :: Proxy base)

-- | Making 'Digit' an instance of 'Num' does not seem
-- sensible, but it's nevertheless useful to be able to
-- add and subtract digits, so '<+>' and '<->' are provided
-- for this.
--
-- @a <+> b@: Add @a@ and @b@. Throws an 'ErrorCall' if the
-- result is out of range.
(<+>) :: KnownNat base => Digit base -> Digit base -> Digit base
a <+> b = digit' $ getVal a + getVal b

-- | @a <-> b@: Subtract @b@ from @a@. Throws an 'ErrorCall' if the
-- result is out of range.
(<->) :: KnownNat base => Digit base -> Digit base -> Digit base
a <-> b = digit' $ getVal a - getVal b

