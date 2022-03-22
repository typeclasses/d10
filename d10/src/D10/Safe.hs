{-# language Safe #-}

{- |

Defines a 'D10' type as
@'D0' | 'D1' | 'D2' | 'D3' | 'D4' | 'D5' | 'D6' | 'D7' | 'D8' | 'D9'@.

-}

module D10.Safe
    (
    -- * Related modules
    -- $modules

    -- * Type
      D10 (..)

    -- ** Bounded
    -- $bounded

    -- ** Enum
    -- $enum

    -- * Quasi-quoters
    , d10list

    -- * Splices
    -- ** Expressions
    , d10ListExp
    -- ** Patterns
    , d10ListPat

    -- * Conversions
    -- ** D10 / Char
    , d10Char, charD10Maybe, charD10Either, charD10Fail
    -- ** D10 / String
    , d10Str, strD10Maybe, strD10Either, strD10Fail
    -- ** [D10] / String
    , strD10ListMaybe, strD10ListEither, strD10ListFail
    -- ** D10 / Natural
    , d10Nat, natD10Maybe, natD10Either, natD10Fail, natMod10
    -- ** D10 / Integer
    , d10Integer, integerD10Maybe, integerD10Either
    , integerD10Fail, integerMod10
    -- ** D10 / Int
    , d10Int, intD10Maybe, intD10Either, intD10Fail, intMod10
    -- ** D10 / general numeric types
    , d10Num, integralD10Maybe, integralD10Either
    , integralD10Fail, integralMod10
    ) where

import D10.Safe.Conversions
import D10.Safe.Quotes
import D10.Safe.Splices
import D10.Safe.Type

{- $modules

Additional functions related to this 'D10' type may be found in:

  * "D10.Safe.Arithmetic"

The contents of the following modules are re-exported here:

  * "D10.Safe.Type"
  * "D10.Safe.Conversions"
  * "D10.Safe.Quotes"
  * "D10.Safe.Splices"

The following modules define @D10@ types in different ways
but are otherwise very similar to this one:

  * "D10.Char"
  * "D10.Num"

This module is called "safe" because, in contrast with the
alternative representations of a digit defined in the other
modules, this 'D10' type does not include any possibility
of representing an invalid non-digit value.

-}

{- $bounded

>>> minBound :: D10
D0

>>> maxBound :: D10
D9

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
