{-# language Safe #-}

module D10.Safe.Arithmetic where

import D10.Safe (D10, d10Int, intMod10)

import qualified Prelude as P

-- | Addition modulo 10.
--
-- >>> D2 + D3
-- D5
--
-- >>> D6 + D7
-- D3

(+) :: D10 -> D10 -> D10
x + y = intMod10 (d10Int x P.+ d10Int y)

-- | Subtraction modulo 10.
--
-- >>> D7 - D5
-- D2
--
-- >>> D3 - D7
-- D6

(-) :: D10 -> D10 -> D10
x - y = intMod10 (d10Int x P.- d10Int y)

-- | Multiplication modulo 10.
--
-- >>> D2 * D4
-- D8
-- >>> D7 * D8
-- D6

(*) :: D10 -> D10 -> D10
x * y = intMod10 (d10Int x P.* d10Int y)
