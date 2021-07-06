{-# language Safe #-}

module D10.Num.Quotes where

import D10.Num.Conversions
import D10.Num.Splices

import Control.Monad ((>=>))
import Control.Monad.Fail (MonadFail (fail))
import Prelude hiding (fail)
import Language.Haskell.TH.Quote (QuasiQuoter (..))

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
    { quoteExp  = strD10Fail >=> (d10Exp . d10Integer @Integer)
    , quotePat  = strD10Fail >=> (d10Pat . d10Integer @Integer)
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
    { quoteExp  = d10ListExp
    , quotePat  = d10ListPat
    , quoteType = \_ -> fail "d10list cannot be used in a type context"
    , quoteDec  = \_ -> fail "d10list cannot be used in a declaration context"
    }
