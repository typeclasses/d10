{-# language Safe #-}

module D10.Char.Arithmetic where

import qualified Prelude as P

import D10.Char (D10, d10Int, intMod10)

-- | Addition modulo 10.
--
-- >>> [d10|2|] + [d10|3|]
-- [d10|5|]
--
-- >>> [d10|6|] + [d10|7|]
-- [d10|3|]

(+) :: D10 -> D10 -> D10
x + y = intMod10 (d10Int x P.+ d10Int y)

-- | Subtraction modulo 10.
--
-- >>> [d10|7|] - [d10|5|]
-- [d10|2|]
--
-- >>> [d10|3|] - [d10|7|]
-- [d10|6|]

(-) :: D10 -> D10 -> D10
x - y = intMod10 (d10Int x P.- d10Int y)

-- | Multiplication modulo 10.
--
-- >>> [d10|2|] * [d10|4|]
-- [d10|8|]
--
-- >>> [d10|7|] * [d10|8|]
-- [d10|6|]

(*) :: D10 -> D10 -> D10
x * y = intMod10 (d10Int x P.* d10Int y)
