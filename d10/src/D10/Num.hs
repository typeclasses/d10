{-# language Trustworthy, TemplateHaskell #-}

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
import D10.Num.Unsafe (D10(..))

-- base
import Control.Monad      ((>=>))
import Control.Monad.Fail (MonadFail (fail))
import Prelude            hiding (fail, (+), (-), (*))

-- template-haskell
import Language.Haskell.TH.Lib    (appE, conE, integerL, litE, litP, varE)
import Language.Haskell.TH.Quote  (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp (..), Pat (..), Q)

{- $modules

The contents of the following modules are re-exported here:

  * "D10.Num.Conversions"

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

-- | Produces an expression of type @'D10' a@ that can be used
-- in a Template Haskell splice.
--
-- >>> d10Nat $(d10Exp 5)
-- 5
--
-- >>> d10Nat $(d10Exp 12)
-- ...
-- ... d10 must be between 0 and 9
-- ...
--
-- You may also be interested in 'd10', a quasi-quoter which
-- does something similar.

d10Exp :: Integer -> Q Exp
d10Exp = integerD10Fail >=> d10Exp'

d10Exp' :: D10 Integer -> Q Exp
d10Exp' (D10_Unsafe x) = conE 'D10_Unsafe `appE` (varE 'fromInteger `appE` litE (integerL x))

-- | Produces an expression of type @['D10' a]@ that can be used
-- in a Template Haskell splice.
--
-- >>> d10Nat <$> $(d10ListExp "")
-- []
--
-- >>> d10Nat <$> $(d10ListExp "5")
-- [5]
--
-- >>> d10Nat <$> $(d10ListExp "58")
-- [5,8]
--
-- >>> d10Nat <$> $(d10ListExp "a")
-- ...
-- ... d10 must be between 0 and 9
-- ...
--
-- You may also be interested in 'd10list', a quasi-quoter which
-- does something similar.

d10ListExp :: String -> Q Exp
d10ListExp = strD10ListFail >=> d10ListExp'

d10ListExp' :: [D10 Integer] -> Q Exp
d10ListExp' =
  foldr
    (\x e -> conE '(:) `appE` d10Exp' x `appE` e)
    (conE '[])

---------------------------------------------------

-- | Produces a pattern that can be used in a splice
-- to match a particular @'D10' a@ value.
--
-- >>> :{
--       case (charD10Maybe '5') of
--         Just $(d10Pat 4) -> "A"
--         Just $(d10Pat 5) -> "B"
--         _                -> "C"
-- >>> :}
-- "B"
--
-- You may wish to use the 'd10' quasi-quoter instead.

d10Pat :: Integer -> Q Pat
d10Pat = integerD10Fail >=> d10Pat'

d10Pat' :: D10 Integer -> Q Pat
d10Pat' (D10_Unsafe x) = [p| D10_Unsafe $(litP $ integerL x) |]

d10Pat'' :: Integral a => D10 a -> Q Pat
d10Pat'' (D10_Unsafe x) = [p| D10_Unsafe $(litP $ integerL $ toInteger x) |]

-- | Produces a pattern that can be used in a splice
-- to match a particular list of @'D10' a@ values.
--
-- >>> :{
--       case (strD10ListMaybe "56") of
--         Just $(d10ListPat "42") -> "A"
--         Just $(d10ListPat "56") -> "B"
--         _                       -> "C"
-- >>> :}
-- "B"
--
-- You may wish to use the 'd10list' quasi-quoter instead.

d10ListPat :: String -> Q Pat
d10ListPat = strD10ListFail >=> d10ListPat'

d10ListPat' :: [D10 Integer] -> Q Pat
d10ListPat' = foldr (\x p -> [p| $(d10Pat'' x) : $(p) |]) [p| [] |]

---------------------------------------------------

-- | A single base-10 digit.
--
-- This quasi-quoter, when used as an expression, produces a
-- value of type @'D10' a@.
--
-- >>> d10Nat [d10|5|]
-- 5
--
-- >>> d10Nat [d10|a|]
-- ...
-- ... d10 must be between 0 and 9
-- ...
--
-- >>> d10Nat [d10|58|]
-- ...
-- ... d10 must be a single character
-- ...
--
-- This quasi-quoter can also be used as a pattern.
--
-- >>> :{
--       case (charD10Maybe '5') of
--         Just [d10|4|] -> "A"
--         Just [d10|5|] -> "B"
--         _             -> "C"
-- >>> :}
-- "B"
--
-- >>> :{
--       case (charD10Maybe '5') of
--         Just [d10|x|] -> "A"
--         Just [d10|5|] -> "B"
--         _             -> "C"
-- >>> :}
-- ...
-- ... d10 must be between 0 and 9
-- ...

d10 :: QuasiQuoter
d10 = QuasiQuoter
    { quoteExp  = strD10Fail >=> d10Exp'
    , quotePat  = strD10Fail >=> d10Pat'
    , quoteType = \_ -> fail "d10 cannot be used in a type context"
    , quoteDec  = \_ -> fail "d10 cannot be used in a declaration context"
    }

-- | A list of base-10 digits.
--
-- This quasi-quoter, when used as an expression, produces a
-- value of type @['D10' a]@.
--
-- >>> d10Nat <$> [d10list||]
-- []
--
-- >>> d10Nat <$> [d10list|5|]
-- [5]
--
-- >>> d10Nat <$> [d10list|58|]
-- [5,8]
--
-- >>> d10Nat <$> [d10list|a|]
-- ...
-- ... d10 must be between 0 and 9
-- ...
--
-- This quasi-quoter can also be used as a pattern.
--
-- >>> :{
--       case [d10list|56|] of
--         [d10list|41|] -> "A"
--         [d10list|56|] -> "B"
--         _             -> "C"
-- >>> :}
-- "B"
--
-- >>> :{
--       case [d10list|56|] of
--         [d10list|4x|] -> "A"
--         [d10list|56|] -> "B"
--         _             -> "C"
-- >>> :}
-- ...
-- ... d10 must be between 0 and 9
-- ...

d10list :: QuasiQuoter
d10list = QuasiQuoter
    { quoteExp  = strD10ListFail >=> d10ListExp'
    , quotePat  = strD10ListFail >=> d10ListPat'
    , quoteType = \_ -> fail "d10list cannot be used in a type context"
    , quoteDec  = \_ -> fail "d10list cannot be used in a declaration context"
    }
