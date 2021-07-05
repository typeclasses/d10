{-# language Trustworthy, TemplateHaskell #-}

-- | Defines a 'D10' type as
-- @'D0' | 'D1' | 'D2' | 'D3' | 'D4' | 'D5' | 'D6' | 'D7' | 'D8' | 'D9'@.
--
-- The following modules define @D10@ types in different ways
-- but are otherwise very similar to this one:
--
-- * "D10.Char"
-- * "D10.Num"
--
-- This module is called "safe" because, in contrast with the
-- alternative representations of a digit defined in the other
-- modules, this 'D10' type does not include any possibility
-- of representing an invalid non-digit value.

module D10.Safe
    (
    -- * Type
      D10 (..)
    -- $bounded
    -- $enum

    -- * Quasi-quoters
    , d10list

    -- * Splice expressions
    , d10ListExp

    -- * Splice patterns
    , d10ListPat

    -- * Converting between D10 and Char
    , d10Char
    , charD10Maybe
    , charD10Either
    , charD10Fail

    -- * Converting between D10 and String
    , d10Str
    , strD10Maybe
    , strD10Either
    , strD10Fail

    -- * Converting between [D10] and String
    , strD10ListMaybe
    , strD10ListEither
    , strD10ListFail

    -- * Converting between D10 and Natural
    , d10Nat
    , natD10Maybe
    , natD10Either
    , natD10Fail
    , natMod10

    -- * Converting between D10 and Integer
    , d10Integer
    , integerD10Maybe
    , integerD10Either
    , integerD10Fail
    , integerMod10

    -- * Converting between D10 and Int
    , d10Int
    , intD10Maybe
    , intD10Either
    , intD10Fail
    , intMod10

    -- * Converting between D10 and general numeric types
    , d10Num
    , integralD10Maybe
    , integralD10Either
    , integralD10Fail
    , integralMod10

    ) where

import D10.Safe.Type (D10 (..))
import D10.Safe.Conversions

-- base
import Control.Monad ((>=>))
import Control.Monad.Fail (MonadFail (fail))
import Prelude hiding (fail, (+), (-), (*))

-- template-haskell
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp (..), Pat (..), Q, dataToPatQ)

---------------------------------------------------

-- $bounded
-- ==== Bounded
--
-- >>> minBound :: D10
-- D0
--
-- >>> maxBound :: D10
-- D9

---------------------------------------------------

-- $enum
-- ==== Enum
--
-- >>> [ D5 .. ]
-- [D5,D6,D7,D8,D9]
--
-- >>> [ D4 .. D7 ]
-- [D4,D5,D6,D7]
--
-- >>> [ D5, D4 .. ]
-- [D5,D4,D3,D2,D1,D0]
--
-- >>> [ D1, D3 .. ]
-- [D1,D3,D5,D7,D9]
--
-- >>> [ minBound .. maxBound ] :: [D10]
-- [D0,D1,D2,D3,D4,D5,D6,D7,D8,D9]

---------------------------------------------------

-- | Produces an expression of type @['D10']@ that can be used
-- in a Template Haskell splice.
--
-- >>> $(d10ListExp "")
-- []
--
-- >>> $(d10ListExp "5")
-- [D5]
--
-- >>> $(d10ListExp "58")
-- [D5,D8]
--
-- >>> $(d10ListExp "a")
-- ...
-- ... d10 must be between 0 and 9
-- ...
--
-- You may also be interested in 'd10list', a quasi-quoter which
-- does something similar.

d10ListExp :: String -> Q Exp
d10ListExp = strD10ListFail >=> d10ListExp'

d10ListExp' :: [D10] -> Q Exp
d10ListExp' x = [| x |]

---------------------------------------------------

d10Pat :: D10 -> Q Pat
d10Pat = dataToPatQ (const Nothing)

-- | Produces a pattern that can be used in a splice
-- to match a particular list of 'D10' values.
--
-- >>> :{
--       case [D5, D6] of
--         $(d10ListPat "42") -> "A"
--         $(d10ListPat "56") -> "B"
--         _                  -> "C"
-- >>> :}
-- "B"
--
-- You may also be interested in 'd10list', a quasi-quoter which
-- does something similar.

d10ListPat :: String -> Q Pat
d10ListPat = strD10ListFail >=> \xs ->
  do
    pats <- traverse d10Pat xs
    return (ListP pats)

d10ListPat' :: [D10] -> Q Pat
d10ListPat' xs =
  do
    pats <- traverse d10Pat xs
    return (ListP pats)

---------------------------------------------------

-- | A list of base-10 digits.
--
-- This quasi-quoter, when used as an expression, produces a
-- value of type @['D10']@.
--
-- >>> [d10list||]
-- []
--
-- >>> [d10list|5|]
-- [D5]
--
-- >>> [d10list|58|]
-- [D5,D8]
--
-- >>> [d10list|a|]
-- ...
-- ... d10 must be between 0 and 9
-- ...
--
-- This quasi-quoter can also be used as a pattern.
--
-- >>> :{
--       case [D5, D6] of
--         [d10list|41|] -> "A"
--         [d10list|56|] -> "B"
--         _             -> "C"
-- >>> :}
-- "B"
--
-- >>> :{
--       case [D5, D6] of
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
