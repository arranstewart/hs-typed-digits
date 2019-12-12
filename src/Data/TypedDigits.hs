{-|

Module      : Data.TypedDigits
Description : Digits, indexed by their base at the type level
Portability : portable

= Description

Digits, indexed by their base at the type level.

-}

module Data.TypedDigits (
    Digit
  -- * Constructing 'Digits'
  , digit
  , digit'
  -- * Querying 'Digits'
  , getVal  
  , getBase
  , getBaseT
  -- * Arithmetic operations on 'Digits'
  , (<+>)
  , (<->)
  , KnownNat
  )
  where

import Data.TypedDigits.Internal

