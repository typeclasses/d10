{-# language Safe #-}

{- |

Defines a 'D10' type as
@'D0' | 'D1' | 'D2' | 'D3' | 'D4' | 'D5' | 'D6' | 'D7' | 'D8' | 'D9'@.

The following modules define @D10@ types in different ways
but are otherwise very similar to this one:

  * "D10.Char"
  * "D10.Num"

This module is called "safe" because, in contrast with the
alternative representations of a digit defined in the other
modules, this 'D10' type does not include any possibility
of representing an invalid non-digit value.

-}

module D10.Safe
    (
    -- * Type
      D10 (..)
    -- ** Bounded
    -- $bounded
    -- ** Enum
    -- $enum
    -- * Quasi-quoters
    , d10list
    -- * Splice expressions
    , d10ListExp
    -- * Splice patterns
    , d10ListPat
    -- * Converting between D10 and Char
    , d10Char, charD10Maybe, charD10Either, charD10Fail
    -- * Converting between D10 and String
    , d10Str, strD10Maybe, strD10Either, strD10Fail
    -- * Converting between [D10] and String
    , strD10ListMaybe, strD10ListEither, strD10ListFail
    -- * Converting between D10 and Natural
    , d10Nat, natD10Maybe, natD10Either, natD10Fail, natMod10
    -- * Converting between D10 and Integer
    , d10Integer, integerD10Maybe, integerD10Either
    , integerD10Fail, integerMod10
    -- * Converting between D10 and Int
    , d10Int, intD10Maybe, intD10Either, intD10Fail, intMod10
    -- * Converting between D10 and general numeric types
    , d10Num, integralD10Maybe, integralD10Either
    , integralD10Fail, integralMod10
    ) where

import D10.Safe.Conversions
import D10.Safe.Quotes
import D10.Safe.Splices
import D10.Safe.Type

import Prelude hiding (fail, (+), (-), (*))

{- $bounded

-- >>> minBound :: D10
-- D0
--
-- >>> maxBound :: D10
-- D9

-}

{- $enum

>>> [ D5 .. ]
[D5,D6,D7,D8,D9]

>>> [ D4 .. D7 ]
[D4,D5,D6,D7]

>>> [ D5, D4 .. ]
[D5,D4,D3,D2,D1,D0]

>>> [ D1, D3 .. ]
[D1,D3,D5,D7,D9]

>>> [ minBound .. maxBound ] :: [D10]
[D0,D1,D2,D3,D4,D5,D6,D7,D8,D9]

-}
