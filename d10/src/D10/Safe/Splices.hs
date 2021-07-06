{-# language Safe #-}

module D10.Safe.Splices
    (
    -- * Splice expressions
      d10ListExp
    -- * Splice patterns
    , d10ListPat
    ) where

import D10.Safe.Conversions (strD10ListFail)

import Control.Monad ((>=>))
import Language.Haskell.TH.Syntax (Exp (..), Pat (..), Q, dataToExpQ, dataToPatQ)
import Prelude hiding (fail, (+), (-), (*))

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
-- You may also be interested in 'D10.Safe.Quotes.d10list',
-- a quasi-quoter which does something similar.

d10ListExp :: String -> Q Exp
d10ListExp = strD10ListFail >=> dataToExpQ (const Nothing)

---------------------------------------------------

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
-- You may also be interested in 'D10.Safe.Quotes.d10list',
-- a quasi-quoter which does something similar.

d10ListPat :: String -> Q Pat
d10ListPat = strD10ListFail >=> \xs ->
  do
    pats <- traverse (dataToPatQ (const Nothing)) xs
    return (ListP pats)
