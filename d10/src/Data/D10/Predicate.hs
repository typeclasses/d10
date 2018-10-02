-- | Functions to test whether values of various
-- types represent digits in the range /0/ to /9/.

module Data.D10.Predicate where

import Numeric.Natural

-- | Determines whether a 'Char' is in the range @'0'@ to @'9'@.

isD10Char :: Char -> Bool
isD10Char x = x >= '0' && x <= '9'

-- | Determines whether a 'String' consists of a single character
-- and that character is within the range @'0'@ to @'9'@.

isD10Str :: String -> Bool
isD10Str [x] = isD10Char x
isD10Str _   = False

-- | Determines whether a 'String' consists entirely of characters
-- that are within the range @'0'@ to @'9'@.

isD10ListStr :: String -> Bool
isD10ListStr = all isD10Char

-- | Determines whether a 'Natural' is in the range 0 to 9.

isD10Nat :: Natural -> Bool
isD10Nat x = x <= 9

-- | Determines whether an 'Integer' is in the range 0 to 9.

isD10Integer :: Integer -> Bool
isD10Integer x = x >= 0 && x <= 9

-- | Determines whether an 'Int' is in the range 0 to 9.

isD10Int :: Int -> Bool
isD10Int x = x >= 0 && x <= 9

-- | Determines whether a number whose type has an 'Integral'
-- instance is in the range 0 to 9.

isD10Integral :: Integral a => a -> Bool
isD10Integral x = isD10Integer (toInteger x)
