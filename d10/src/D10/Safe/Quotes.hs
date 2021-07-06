{-# language Safe #-}

module D10.Safe.Quotes (d10list) where

import D10.Safe.Splices

import Control.Monad.Fail (MonadFail (fail))
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Prelude hiding (fail, (+), (-), (*))

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
    { quoteExp  = d10ListExp
    , quotePat  = d10ListPat
    , quoteType = \_ -> fail "d10list cannot be used in a type context"
    , quoteDec  = \_ -> fail "d10list cannot be used in a declaration context"
    }
