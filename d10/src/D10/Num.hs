{-# language Safe #-}

{- |

Defines a 'D10' type as a newtype for any type with an
instance of the 'Num' class, where the values are restricted
to numbers between @'fromInteger' 0@ and @'fromInteger' 9@.

This module provides many functions for constructing 'D10'
values, including:

  * @'integerD10Maybe' :: 'Num' a => 'Integer' -> 'Maybe' ('D10' a)@
  * @'integerMod10' :: 'Num' a => 'Integer' -> 'D10' a@

There are also several ways to safely write 'D10' literals
using Template Haskell:

  * With the @QuasiQuotes@ GHC extension enabled, you can write
    use the quasi-quoters 'd10' and 'd10list'.
  * With the @TemplateHaskell@ GHC extension enabled, you can
    splice expressions produced by 'd10Exp' and 'd10ListExp'.

-}

module D10.Num
    (
    -- * Related modules
    -- $modules

    -- * Type
      D10

    -- ** Bounded
    -- $bounded

    -- ** Enum
    -- $enum

    -- ** Show
    -- $show

    -- * Quasi-quoters
    , d10, d10list

    -- * Splices
    -- ** Expressions
    , d10Exp, d10ListExp
    -- ** Patterns
    , d10Pat, d10ListPat

    -- * Conversions
    -- ** D10 / Char
    , d10Char, charD10Maybe, charD10Either, charD10Fail
    -- ** D10 / String
    , d10Str, strD10Maybe, strD10Either, strD10Fail
    -- ** [D10] / String
    , strD10ListMaybe, strD10ListEither, strD10ListFail
    -- ** D10 / Natural
    , d10Nat, natD10Maybe, natD10Either, natD10Fail
    , natMod10
    -- ** D10 / Integer
    , d10Integer, integerD10Maybe, integerD10Either
    , integerD10Fail, integerMod10
    -- ** D10 / Int
    , d10Int, intD10Maybe, intD10Either, intD10Fail
    , intMod10
    -- ** D10 / general numeric types
    , d10Num, integralD10Maybe, integralD10Either
    , integralD10Fail, integralMod10

    ) where

import D10.Num.Conversions
import D10.Num.Type
import D10.Num.Quotes
import D10.Num.Splices

{- $modules

Additional functions related to this 'D10' type may be found in:

  * "D10.Num.Arithmetic"

The contents of the following modules are re-exported here:

  * "D10.Num.Conversions"
  * "D10.Num.Quotes"
  * "D10.Num.Splices"
  * "D10.Num.Type"

The unsafe constructor for 'D10' can be found in:

  * "D10.Num.Unsafe"

The following modules define @D10@ types in different ways
but are otherwise very similar to this one:

  * "D10.Char"
  * "D10.Safe"

-}

{- $bounded

>>> minBound :: D10 Integer
[d10|0|]

>>> maxBound :: D10 Integer
[d10|9|]

-}

{- $enum

>>> [ [d10|5|] .. ]
[d10list|56789|]

>>> [ [d10|4|] .. [d10|7|] ]
[d10list|4567|]

>>> [ [d10|5|], [d10|4|] .. ]
[d10list|543210|]

>>> [ [d10|1|], [d10|3|] .. ]
[d10list|13579|]

>>> [ minBound .. maxBound ] :: [D10 Integer]
[d10list|0123456789|]

-}

{- $show

'show' shows base-10 digits using the quasiquoters defined
in this module. A single digit is displayed using 'd10'.
A list of digits is displayed using 'd10list'.

-}
