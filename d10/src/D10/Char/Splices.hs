{-# language Trustworthy, TemplateHaskell #-}

module D10.Char.Splices
    (
    -- * Expressions
      d10Exp, d10ListExp
    -- * Patterns
    , d10Pat, d10ListPat
    ) where

import D10.Char.Conversions
import D10.Char.Unsafe

import Control.Monad ((>=>))
import Language.Haskell.TH.Lib (appE, charL, conE, litE, litP)
import Language.Haskell.TH.Syntax (Exp (..), Pat (..), Q)

-- | Produces an expression of type 'D10' that can be used
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

d10Exp' :: D10 -> Q Exp
d10Exp' x = conE 'D10_Unsafe `appE` litE (charL (d10Char x))

-- | Produces an expression of type @['D10']@ that can be used
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

d10ListExp' :: [D10] -> Q Exp
d10ListExp' =
  foldr
    (\x e -> conE '(:) `appE` d10Exp' x `appE` e)
    (conE '[])

---------------------------------------------------

-- | Produces a pattern that can be used in a splice
-- to match a particular 'D10' value.
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

d10Pat' :: D10 -> Q Pat
d10Pat' (D10_Unsafe x) = [p| D10_Unsafe $(litP $ charL x) |]

-- | Produces a pattern that can be used in a splice
-- to match a particular list of 'D10' values.
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
d10ListPat = strD10ListFail >=> foldr (\x p -> [p| $(d10Pat' x) : $(p) |]) [p| [] |]
