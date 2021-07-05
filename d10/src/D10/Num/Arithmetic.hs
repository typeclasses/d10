{-# language Safe #-}

module D10.Num.Arithmetic where

import D10.Num (D10, d10Int, intMod10)

import qualified Prelude as P
import Prelude (Integral)

-- | Addition modulo 10.
--
-- >>> [d10|2|] + [d10|3|]
-- [d10|5|]
--
-- >>> [d10|6|] + [d10|7|]
-- [d10|3|]

(+) :: Integral a => D10 a -> D10 a -> D10 a
x + y = intMod10 (d10Int x P.+ d10Int y)

-- | Subtraction modulo 10.
--
-- >>> [d10|7|] - [d10|5|]
-- [d10|2|]
--
-- >>> [d10|3|] - [d10|7|]
-- [d10|6|]

(-) :: Integral a => D10 a -> D10 a -> D10 a
x - y = intMod10 (d10Int x P.- d10Int y)

-- | Multiplication modulo 10.
--
-- >>> [d10|2|] * [d10|4|]
-- [d10|8|]
--
-- >>> [d10|7|] * [d10|8|]
-- [d10|6|]

(*) :: Integral a => D10 a -> D10 a -> D10 a
x * y = intMod10 (d10Int x P.* d10Int y)
