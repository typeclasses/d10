{-# language Safe #-}

module D10.Safe.Conversions
    (
    -- * Converting between D10 and Char
      d10Char, charD10Maybe, charD10Either, charD10Fail
    -- * Converting between D10 and String
    , d10Str, strD10Maybe, strD10Either, strD10Fail
    -- * Converting between [D10] and String
    , strD10ListMaybe, strD10ListEither, strD10ListFail
    -- * Converting between D10 and Natural
    , d10Nat, natD10Maybe, natD10Either, natD10Fail, natMod10
    -- * Converting between D10 and Integer
    , d10Integer, integerD10Maybe, integerD10Either
    , integerD10Fail, integerMod10
    -- * Converting between D10 and Int
    , d10Int, intD10Maybe, intD10Either, intD10Fail, intMod10
    -- * Converting between D10 and general numeric types
    , d10Num, integralD10Maybe, integralD10Either
    , integralD10Fail, integralMod10
    ) where

import D10.Safe.Type (D10 (..))

-- base
import Control.Monad.Fail (MonadFail (fail))
import Numeric.Natural    (Natural)
import Prelude            hiding (fail, (+), (-), (*))

-- | Convert a 'D10' to its underlying 'Char' representation.
--
-- >>> d10Char D7
-- '7'

d10Char :: D10 -> Char
d10Char x =
    case x of
        D0 -> '0'
        D1 -> '1'
        D2 -> '2'
        D3 -> '3'
        D4 -> '4'
        D5 -> '5'
        D6 -> '6'
        D7 -> '7'
        D8 -> '8'
        D9 -> '9'

-- | Convert a 'D10' to a 'String'.
--
-- @'d10Str' x = ['d10Char' x]@
--
-- >>> d10Str D7
-- "7"

d10Str :: D10 -> String
d10Str x = [d10Char x]

-- | Convert a 'D10' to a 'Natural'.
--
-- 'd10Num' is a more general version of this function.
--
-- >>> d10Nat D7
-- 7

d10Nat :: D10 -> Natural
d10Nat = d10Num

-- | Convert a 'D10' to an 'Integer'.
--
-- 'd10Num' is a more general version of this function.
--
-- >>> d10Integer D7
-- 7

d10Integer :: D10 -> Integer
d10Integer = d10Num

-- | Convert a 'D10' to an 'Int'.
--
-- 'd10Num' is a more general version of this function.
--
-- >>> d10Int D7
-- 7

d10Int :: D10 -> Int
d10Int = d10Num

-- | Convert a 'D10' to any kind of number with a 'Num' instance.
--
-- Specialized versions of this function include 'd10Nat',
-- 'd10Integer', and 'd10Int'.
--
-- >>> d10Num D7 :: Integer
-- 7

d10Num :: Num a => D10 -> a
d10Num x =
    case x of
        D0 -> 0
        D1 -> 1
        D2 -> 2
        D3 -> 3
        D4 -> 4
        D5 -> 5
        D6 -> 6
        D7 -> 7
        D8 -> 8
        D9 -> 9

---------------------------------------------------

-- | The 'D10' which is uniquely congruent modulo 10 to the given 'Natural'.
--
-- 'integralMod10' is a more general version of this function.
--
-- >>> natMod10 56
-- D6

natMod10 :: Natural -> D10
natMod10 = integralMod10

-- | The 'D10' which is uniquely congruent modulo 10 to the given 'Integer'.
--
-- 'integralMod10' is a more general version of this function.
--
-- >>> integerMod10 56
-- D6
--
-- >>> integerMod10 (-56)
-- D4

integerMod10 :: Integer -> D10
integerMod10 = integralMod10

-- | The 'D10' which is uniquely congruent modulo 10 to the given 'Int'.
--
-- 'integralMod10' is a more general version of this function.
--
-- >>> intMod10 56
-- D6
--
-- >>> intMod10 (-56)
-- D4

intMod10 :: Int -> D10
intMod10 = integralMod10

-- | The 'D10' which is uniquely congruent modulo 10 to the given number
-- (whose type must have an instance of the 'Integral' class).
--
-- Specialized versions of this function include 'natMod10',
-- 'integerMod10', and 'intMod10'.
--
-- >>> integralMod10 (56 :: Integer)
-- D6
--
-- >>> integralMod10 ((-56) :: Integer)
-- D4

integralMod10 :: Integral a => a -> D10
integralMod10 x =
    case (x `mod` 10) of
        0 -> D0
        1 -> D1
        2 -> D2
        3 -> D3
        4 -> D4
        5 -> D5
        6 -> D6
        7 -> D7
        8 -> D8
        9 -> D9
        _ -> error "x `mod` 10 is not between 0 and 9"

---------------------------------------------------

-- | Convert a 'Char' to a 'D10' if it is within the range
-- @'0'@ to @'9'@, or produce 'Nothing' otherwise.
--
-- @'D10.Predicate.isD10Char' x = 'Data.Maybe.isJust' ('charD10Maybe' x)@
--
-- 'charD10Fail' is a more general version of this function.
--
-- >>> charD10Maybe '5'
-- Just D5
--
-- >>> charD10Maybe 'a'
-- Nothing

charD10Maybe :: Char -> Maybe D10
charD10Maybe x =
    case x of
        '0' -> Just D0
        '1' -> Just D1
        '2' -> Just D2
        '3' -> Just D3
        '4' -> Just D4
        '5' -> Just D5
        '6' -> Just D6
        '7' -> Just D7
        '8' -> Just D8
        '9' -> Just D9
        _   -> Nothing

-- | Convert a 'String' to a 'D10' if it consists of exactly one
-- character and that character is within the range @'0'@ to @'9'@,
-- or produce 'Nothing' otherwise.
--
-- @'D10.Predicate.isD10Str' x = 'Data.Maybe.isJust' ('strD10Maybe' x)@
--
-- 'strD10Fail' is a more general version of this function.
--
-- >>> strD10Maybe "5"
-- Just D5
--
-- >>> strD10Maybe "a"
-- Nothing
--
-- >>> strD10Maybe "58"
-- Nothing

strD10Maybe :: String -> Maybe D10
strD10Maybe [x] = charD10Maybe x
strD10Maybe _   = Nothing

-- | Convert a 'String' to a list of 'D10' if all of the characters
-- in the string are within the range @'0'@ to @'9'@, or produce
-- 'Nothing' otherwise.
--
-- @'D10.Predicate.isD10ListStr' x = 'Data.Maybe.isJust' ('strD10ListMaybe' x)@
--
-- 'strD10ListFail' is a more general version of this function.
--
-- >>> strD10ListMaybe "5"
-- Just [D5]
--
-- >>> strD10ListMaybe "a"
-- Nothing
--
-- >>> strD10ListMaybe "58"
-- Just [D5,D8]

strD10ListMaybe :: String -> Maybe [D10]
strD10ListMaybe = traverse charD10Maybe

-- | Convert a 'Natural' to a 'D10' if it is less than 10,
-- or produce 'Nothing' otherwise.
--
-- @'D10.Predicate.isD10Nat' x = 'Data.Maybe.isJust' ('natD10Maybe' x)@
--
-- 'integralD10Maybe', 'natD10Fail', and 'integralD10Fail'
-- are more general versions of this function.
--
-- >>> natD10Maybe 5
-- Just D5
--
-- >>> natD10Maybe 12
-- Nothing

natD10Maybe :: Natural -> Maybe D10
natD10Maybe = integralD10Maybe

-- | Convert an 'Integer' to a 'D10' if it is within the range 0 to 9,
-- or produce 'Nothing' otherwise.
--
-- @'D10.Predicate.isD10Integer' x = 'Data.Maybe.isJust' ('integerD10Maybe' x)@
--
-- 'integralD10Maybe', 'integerD10Fail', and 'integralD10Fail'
-- are more general versions of this function.
--
-- >>> integerD10Maybe 5
-- Just D5
--
-- >>> integerD10Maybe 12
-- Nothing
--
-- >>> integerD10Maybe (-5)
-- Nothing

integerD10Maybe :: Integer -> Maybe D10
integerD10Maybe = integralD10Maybe

-- | Convert an 'Int' to a 'D10' if it is within the range 0 to 9,
-- or produce 'Nothing' otherwise.
--
-- @'D10.Predicate.isD10Int' x = 'Data.Maybe.isJust' ('intD10Maybe' x)@
--
-- 'integralD10Maybe', 'intD10Fail', and 'integralD10Fail'
-- are more general versions of this function.
--
-- >>> intD10Maybe 5
-- Just D5
--
-- >>> intD10Maybe 12
-- Nothing
--
-- >>> intD10Maybe (-5)
-- Nothing

intD10Maybe :: Int -> Maybe D10
intD10Maybe = integralD10Maybe

-- | Construct a 'D10' from any kind of number with an 'Integral'
-- instance, or produce 'Nothing' if the number falls outside the
-- range 0 to 9.
--
-- @'D10.Predicate.isD10Integral' x = 'Data.Maybe.isJust' ('integralD10Maybe' x)@
--
-- Specialized versions of this function include 'natD10Maybe',
-- 'integerD10Maybe', and 'intD10Maybe'.
--
-- 'integralD10Fail' is a more general version of this function.
--
-- >>> integralD10Maybe (5 :: Integer)
-- Just D5
--
-- >>> integralD10Maybe (12 :: Integer)
-- Nothing
--
-- >>> integralD10Maybe ((-5) :: Integer)
-- Nothing

integralD10Maybe :: Integral a => a -> Maybe D10
integralD10Maybe x =
    case x of
        0 -> Just D0
        1 -> Just D1
        2 -> Just D2
        3 -> Just D3
        4 -> Just D4
        5 -> Just D5
        6 -> Just D6
        7 -> Just D7
        8 -> Just D8
        9 -> Just D9
        _ -> Nothing

---------------------------------------------------

-- | Convert a 'Char' to a 'D10' if it is within the range
-- @'0'@ to @'9'@, or 'Left' with an error message otherwise.
--
-- >>> charD10Either '5'
-- Right D5
--
-- >>> charD10Either 'a'
-- Left "d10 must be between 0 and 9"

charD10Either :: Char -> Either String D10
charD10Either x =
    case x of
        '0' -> Right D0
        '1' -> Right D1
        '2' -> Right D2
        '3' -> Right D3
        '4' -> Right D4
        '5' -> Right D5
        '6' -> Right D6
        '7' -> Right D7
        '8' -> Right D8
        '9' -> Right D9
        _   -> Left "d10 must be between 0 and 9"

-- | Convert a 'String' to a 'D10' if it consists of a single
-- character and that character is within the range @'0'@ to
-- @'9'@, or 'Left' with an error message otherwise.
--
-- >>> strD10Either "5"
-- Right D5
--
-- >>> strD10Either "a"
-- Left "d10 must be between 0 and 9"
--
-- >>> strD10Either "58"
-- Left "d10 must be a single character"

strD10Either :: String -> Either String D10
strD10Either [x] = charD10Either x
strD10Either _   = Left "d10 must be a single character"

-- | Convert a 'String' to a 'D10' if all of the characters in
-- the string fall within the range @'0'@ to @'9'@, or 'Left'
-- with an error message otherwise.
--
-- >>> strD10ListEither "5"
-- Right [D5]
--
-- >>> strD10ListEither "a"
-- Left "d10 must be between 0 and 9"
--
-- >>> strD10ListEither "58"
-- Right [D5,D8]

strD10ListEither :: String -> Either String [D10]
strD10ListEither = traverse charD10Either

-- | Convert a 'Natural' to a 'D10' if it is less than 10,
-- or 'Left' with an error message otherwise.
--
-- >>> natD10Either 5
-- Right D5
--
-- >>> natD10Either 12
-- Left "d10 must be less than 10"

natD10Either :: Natural -> Either String D10
natD10Either x =
    case x of
        0 -> Right D0
        1 -> Right D1
        2 -> Right D2
        3 -> Right D3
        4 -> Right D4
        5 -> Right D5
        6 -> Right D6
        7 -> Right D7
        8 -> Right D8
        9 -> Right D9
        _ -> Left "d10 must be less than 10"

-- | Convert an 'Integer' to a 'D10' if it is within the
-- range 0 to 9, or 'Left' with an error message otherwise.
--
-- >>> integerD10Either 5
-- Right D5
--
-- >>> integerD10Either 12
-- Left "d10 must be between 0 and 9"
--
-- >>> integerD10Either (-5)
-- Left "d10 must be between 0 and 9"

integerD10Either :: Integer -> Either String D10
integerD10Either = integralD10Either

-- | Convert an 'Int' to a 'D10' if it is within the range
-- 0 to 9, or 'Left' with an error message otherwise.
--
-- >>> intD10Either 5
-- Right D5
--
-- >>> intD10Either 12
-- Left "d10 must be between 0 and 9"
--
-- >>> intD10Either (-5)
-- Left "d10 must be between 0 and 9"

intD10Either :: Int -> Either String D10
intD10Either = integralD10Either

-- | Convert a number of a type that has an 'Integral' instance
-- to a 'D10' if it falls within the range 0 to 9, or 'Left'
-- with an error message otherwise.
--
-- >>> integralD10Either (5 :: Integer)
-- Right D5
--
-- >>> integralD10Either (12 :: Integer)
-- Left "d10 must be between 0 and 9"
--
-- >>> integralD10Either ((-5) :: Integer)
-- Left "d10 must be between 0 and 9"

integralD10Either :: Integral a => a -> Either String D10
integralD10Either x =
    case (integralD10Maybe x) of
        Just y  -> Right y
        Nothing -> Left "d10 must be between 0 and 9"

---------------------------------------------------

-- | Convert a 'Char' to a 'D10' if it is within the range
-- @'0'@ to @'9'@, or 'fail' with an error message otherwise.
--
-- 'charD10Maybe' is a specialized version of this function.
--
-- >>> charD10Fail '5' :: IO D10
-- D5
--
-- >>> charD10Fail 'a' :: IO D10
-- *** Exception: user error (d10 must be between 0 and 9)

charD10Fail :: MonadFail m => Char -> m D10
charD10Fail x =
    case x of
        '0' -> return D0
        '1' -> return D1
        '2' -> return D2
        '3' -> return D3
        '4' -> return D4
        '5' -> return D5
        '6' -> return D6
        '7' -> return D7
        '8' -> return D8
        '9' -> return D9
        _   -> fail "d10 must be between 0 and 9"

-- | Convert a 'String' to a 'D10' if it consists of a single
-- character and that character is within the range @'0'@ to
-- @'9'@, or 'fail' with an error message otherwise.
--
-- 'strD10Maybe' is a specialized version of this function.
--
-- >>> strD10Fail "5" :: IO D10
-- D5
--
-- >>> strD10Fail "a" :: IO D10
-- *** Exception: user error (d10 must be between 0 and 9)
--
-- >>> strD10Fail "58" :: IO D10
-- *** Exception: user error (d10 must be a single character)

strD10Fail :: MonadFail m => String -> m D10
strD10Fail [x] = charD10Fail x
strD10Fail _   = fail "d10 must be a single character"

-- | Convert a 'String' to a 'D10' if all of the characters in
-- the string fall within the range @'0'@ to @'9'@, or 'fail'
-- with an error message otherwise.
--
-- 'strD10ListMaybe' is a specialized version of this function.
--
-- >>> strD10ListFail "5" :: IO [D10]
-- [D5]
--
-- >>> strD10ListFail "a" :: IO [D10]
-- *** Exception: user error (d10 must be between 0 and 9)
--
-- >>> strD10ListFail "58" :: IO [D10]
-- [D5,D8]

strD10ListFail :: MonadFail m => String -> m [D10]
strD10ListFail = traverse charD10Fail

-- | Convert a 'Natural' to a 'D10' if it is less than 10,
-- or 'fail' with an error message otherwise.
--
-- 'natD10Maybe' is a specialized version of this function.
--
-- 'integralD10Fail' is a more general version of this function.
--
-- >>> natD10Fail 5 :: IO D10
-- D5
--
-- >>> natD10Fail 12 :: IO D10
-- *** Exception: user error (d10 must be less than 10)

natD10Fail :: MonadFail m => Natural -> m D10
natD10Fail x =
    case x of
        0 -> return D0
        1 -> return D1
        2 -> return D2
        3 -> return D3
        4 -> return D4
        5 -> return D5
        6 -> return D6
        7 -> return D7
        8 -> return D8
        9 -> return D9
        _ -> fail "d10 must be less than 10"

-- | Convert an 'Integer' to a 'D10' if it is within the
-- range 0 to 9, or 'fail' with an error message otherwise.
--
-- 'integerD10Maybe' is a specialized version of this function.
--
-- 'integralD10Fail' is a more general version of this function.
--
-- >>> integerD10Fail 5 :: IO D10
-- D5
--
-- >>> integerD10Fail 12 :: IO D10
-- *** Exception: user error (d10 must be between 0 and 9)
--
-- >>> integerD10Fail (-5) :: IO D10
-- *** Exception: user error (d10 must be between 0 and 9)

integerD10Fail :: MonadFail m => Integer -> m D10
integerD10Fail = integralD10Fail

-- | Convert an 'Int' to a 'D10' if it is within the range
-- 0 to 9, or 'fail' with an error message otherwise.
--
-- 'intD10Maybe' is a specialized version of this function.
--
-- 'integralD10Fail' is a more general version of this function.
--
-- >>> intD10Fail 5 :: IO D10
-- D5
--
-- >>> intD10Fail 12 :: IO D10
-- *** Exception: user error (d10 must be between 0 and 9)
--
-- >>> intD10Fail (-5) :: IO D10
-- *** Exception: user error (d10 must be between 0 and 9)

intD10Fail :: MonadFail m => Int -> m D10
intD10Fail = integralD10Fail

-- | Convert a number of a type that has an 'Integral' instance
-- to a 'D10' if it falls within the range 0 to 9, or 'fail'
-- with an error message otherwise.
--
-- 'natD10Maybe', 'integerD10Maybe', 'intD10Maybe',
-- 'integralD10Maybe', 'natD10Fail', 'integerD10Fail', and
-- 'intD10Fail' are all specialized versions of this function.
--
-- >>> integralD10Fail (5 :: Integer) :: IO D10
-- D5
--
-- >>> integralD10Fail (12 :: Integer) :: IO D10
-- *** Exception: user error (d10 must be between 0 and 9)
--
-- >>> integralD10Fail ((-5) :: Integer) :: IO D10
-- *** Exception: user error (d10 must be between 0 and 9)

integralD10Fail :: (Integral a, MonadFail m) => a -> m D10
integralD10Fail x =
    case (integralD10Maybe x) of
        Just y  -> return y
        Nothing -> fail "d10 must be between 0 and 9"
