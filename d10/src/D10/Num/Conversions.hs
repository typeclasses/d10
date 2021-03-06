{-# language Trustworthy #-}

module D10.Num.Conversions
    (
    -- * D10 / Char
      d10Char, charD10Maybe, charD10Either, charD10Fail
    -- * D10 / String
    , d10Str, strD10Maybe, strD10Either, strD10Fail
    -- * [D10] / String
    , strD10ListMaybe, strD10ListEither, strD10ListFail
    -- * D10 / Natural
    , d10Nat, natD10Maybe, natD10Either, natD10Fail
    , natMod10
    -- * D10 / Integer
    , d10Integer, integerD10Maybe, integerD10Either
    , integerD10Fail, integerMod10
    -- * D10 / Int
    , d10Int, intD10Maybe, intD10Either, intD10Fail
    , intMod10
    -- * D10 / general numeric types
    , d10Num, integralD10Maybe, integralD10Either
    , integralD10Fail, integralMod10
    ) where

import D10.Num.Unsafe (D10(..))
import D10.Predicate (isD10Char, isD10Int, isD10Integer, isD10Nat)

-- base
import Control.Monad.Fail (MonadFail (fail))
import Data.Char          (chr, ord)
import Numeric.Natural    (Natural)
import Prelude            hiding (fail)

import qualified Prelude as P

-- | Convert a 'D10' to its underlying 'Char' representation.
--
-- >>> d10Char [d10|7|]
-- '7'

d10Char :: Integral a => D10 a -> Char
d10Char (D10_Unsafe x) = chr (ord '0' P.+ fromIntegral x)

-- | Convert a 'D10' to a 'String'.
--
-- @'d10Str' x = ['d10Char' x]@
--
-- >>> d10Str [d10|7|]
-- "7"

d10Str :: Integral a => D10 a -> String
d10Str x = [d10Char x]

-- | Convert a 'D10' to a 'Natural'.
--
-- 'd10Num' is a more general version of this function.
--
-- >>> d10Nat [d10|7|]
-- 7

d10Nat :: Integral a => D10 a -> Natural
d10Nat = d10Num

-- | Convert a 'D10' to an 'Integer'.
--
-- 'd10Num' is a more general version of this function.
--
-- >>> d10Integer [d10|7|]
-- 7

d10Integer :: Integral a => D10 a -> Integer
d10Integer = d10Num

-- | Convert a 'D10' to an 'Int'.
--
-- 'd10Num' is a more general version of this function.
--
-- >>> d10Int [d10|7|]
-- 7

d10Int :: Integral a => D10 a -> Int
d10Int = d10Num

-- | Convert a 'D10' to any kind of number with a 'Num' instance.
--
-- Specialized versions of this function include 'd10Nat',
-- 'd10Integer', and 'd10Int'.
--
-- >>> d10Num [d10|7|] :: Integer
-- 7

d10Num :: (Integral b, Num a) => D10 b -> a
d10Num (D10_Unsafe x) = fromIntegral x

---------------------------------------------------

-- | The 'D10' which is uniquely congruent modulo 10 to the given 'Natural'.
--
-- 'integralMod10' is a more general version of this function.
--
-- >>> natMod10 56 :: D10 Int
-- [d10|6|]

natMod10 :: Num a => Natural -> D10 a
natMod10 = integralMod10

-- | The 'D10' which is uniquely congruent modulo 10 to the given 'Integer'.
--
-- 'integralMod10' is a more general version of this function.
--
-- >>> integerMod10 56 :: D10 Int
-- [d10|6|]
--
-- >>> integerMod10 (-56) :: D10 Int
-- [d10|4|]

integerMod10 :: Num a => Integer -> D10 a
integerMod10 = integralMod10

-- | The 'D10' which is uniquely congruent modulo 10 to the given 'Int'.
--
-- 'integralMod10' is a more general version of this function.
--
-- >>> intMod10 56 :: D10 Int
-- [d10|6|]
--
-- >>> intMod10 (-56) :: D10 Int
-- [d10|4|]

intMod10 :: Num a => Int -> D10 a
intMod10 = integralMod10

-- | The 'D10' which is uniquely congruent modulo 10 to the given number
-- (whose type must have an instance of the 'Integral' class).
--
-- Specialized versions of this function include 'natMod10',
-- 'integerMod10', and 'intMod10'.
--
-- >>> integralMod10 (56 :: Integer) :: D10 Int
-- [d10|6|]
--
-- >>> integralMod10 ((-56) :: Integer) :: D10 Int
-- [d10|4|]

integralMod10 :: (Num b, Integral a) => a -> D10 b
integralMod10 x = D10_Unsafe (fromIntegral (x `mod` 10))

---------------------------------------------------

-- | Convert a 'Char' to a 'D10' if it is within the range
-- @'0'@ to @'9'@, or produce 'Nothing' otherwise.
--
-- @'D10.Predicate.isD10Char' x = 'Data.Maybe.isJust' ('charD10Maybe' x)@
--
-- 'charD10Fail' is a more general version of this function.
--
-- >>> charD10Maybe '5'
-- Just [d10|5|]
--
-- >>> charD10Maybe 'a'
-- Nothing

charD10Maybe :: Num a => Char -> Maybe (D10 a)
charD10Maybe x
        | isD10Char x  =  Just (D10_Unsafe (fromIntegral (ord x P.- ord '0')))
        | otherwise    =  Nothing

-- | Convert a 'String' to a 'D10' if it consists of exactly one
-- character and that character is within the range @'0'@ to @'9'@,
-- or produce 'Nothing' otherwise.
--
-- @'D10.Predicate.isD10Str' x = 'Data.Maybe.isJust' ('strD10Maybe' x)@
--
-- 'strD10Fail' is a more general version of this function.
--
-- >>> strD10Maybe "5"
-- Just [d10|5|]
--
-- >>> strD10Maybe "a"
-- Nothing
--
-- >>> strD10Maybe "58"
-- Nothing

strD10Maybe :: Num a => String -> Maybe (D10 a)
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
-- Just [d10list|5|]
--
-- >>> strD10ListMaybe "a"
-- Nothing
--
-- >>> strD10ListMaybe "58"
-- Just [d10list|58|]

strD10ListMaybe :: Num a => String -> Maybe [D10 a]
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
-- Just [d10|5|]
--
-- >>> natD10Maybe 12
-- Nothing

natD10Maybe :: Num a => Natural -> Maybe (D10 a)
natD10Maybe x
        | isD10Nat x  =  Just (D10_Unsafe (fromIntegral x))
        | otherwise   =  Nothing

-- | Convert an 'Integer' to a 'D10' if it is within the range 0 to 9,
-- or produce 'Nothing' otherwise.
--
-- @'D10.Predicate.isD10Integer' x = 'Data.Maybe.isJust' ('integerD10Maybe' x)@
--
-- 'integralD10Maybe', 'integerD10Fail', and 'integralD10Fail'
-- are more general versions of this function.
--
-- >>> integerD10Maybe 5
-- Just [d10|5|]
--
-- >>> integerD10Maybe 12
-- Nothing
--
-- >>> integerD10Maybe (-5)
-- Nothing

integerD10Maybe :: Num a => Integer -> Maybe (D10 a)
integerD10Maybe x
        | isD10Integer x  =  Just (D10_Unsafe (fromIntegral x))
        | otherwise       =  Nothing

-- | Convert an 'Int' to a 'D10' if it is within the range 0 to 9,
-- or produce 'Nothing' otherwise.
--
-- @'D10.Predicate.isD10Int' x = 'Data.Maybe.isJust' ('intD10Maybe' x)@
--
-- 'integralD10Maybe', 'intD10Fail', and 'integralD10Fail'
-- are more general versions of this function.
--
-- >>> intD10Maybe 5
-- Just [d10|5|]
--
-- >>> intD10Maybe 12
-- Nothing
--
-- >>> intD10Maybe (-5)
-- Nothing

intD10Maybe :: Num a => Int -> Maybe (D10 a)
intD10Maybe x
        | isD10Int x  =  Just (D10_Unsafe (fromIntegral x))
        | otherwise   =  Nothing

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
-- Just [d10|5|]
--
-- >>> integralD10Maybe (12 :: Integer)
-- Nothing
--
-- >>> integralD10Maybe ((-5) :: Integer)
-- Nothing

integralD10Maybe :: (Num b, Integral a) => a -> Maybe (D10 b)
integralD10Maybe x = integerD10Maybe (toInteger x)

---------------------------------------------------

-- | Convert a 'Char' to a 'D10' if it is within the range
-- @'0'@ to @'9'@, or 'Left' with an error message otherwise.
--
-- >>> charD10Either '5'
-- Right [d10|5|]
--
-- >>> charD10Either 'a'
-- Left "d10 must be between 0 and 9"

charD10Either :: Num a => Char -> Either String (D10 a)
charD10Either x
        | isD10Char x  =  Right (D10_Unsafe (fromIntegral (ord x P.- ord '0')))
        | otherwise    =  Left "d10 must be between 0 and 9"

-- | Convert a 'String' to a 'D10' if it consists of a single
-- character and that character is within the range @'0'@ to
-- @'9'@, or 'Left' with an error message otherwise.
--
-- >>> strD10Either "5"
-- Right [d10|5|]
--
-- >>> strD10Either "a"
-- Left "d10 must be between 0 and 9"
--
-- >>> strD10Either "58"
-- Left "d10 must be a single character"

strD10Either :: Num a => String -> Either String (D10 a)
strD10Either [x]         =  charD10Either x
strD10Either _           =  Left "d10 must be a single character"

-- | Convert a 'String' to a 'D10' if all of the characters in
-- the string fall within the range @'0'@ to @'9'@, or 'Left'
-- with an error message otherwise.
--
-- >>> strD10ListEither "5"
-- Right [d10list|5|]
--
-- >>> strD10ListEither "a"
-- Left "d10 must be between 0 and 9"
--
-- >>> strD10ListEither "58"
-- Right [d10list|58|]

strD10ListEither :: Num a => String -> Either String [D10 a]
strD10ListEither = traverse charD10Either

-- | Convert a 'Natural' to a 'D10' if it is less than 10,
-- or 'Left' with an error message otherwise.
--
-- >>> natD10Either 5
-- Right [d10|5|]
--
-- >>> natD10Either 12
-- Left "d10 must be less than 10"

natD10Either :: Num a => Natural -> Either String (D10 a)
natD10Either x =
    case (natD10Maybe x) of
        Just y  -> Right y
        Nothing -> Left "d10 must be less than 10"

-- | Convert an 'Integer' to a 'D10' if it is within the
-- range 0 to 9, or 'Left' with an error message otherwise.
--
-- >>> integerD10Either 5
-- Right [d10|5|]
--
-- >>> integerD10Either 12
-- Left "d10 must be between 0 and 9"
--
-- >>> integerD10Either (-5)
-- Left "d10 must be between 0 and 9"

integerD10Either :: Num a => Integer -> Either String (D10 a)
integerD10Either x =
    case (integerD10Maybe x) of
        Just y  -> Right y
        Nothing -> Left "d10 must be between 0 and 9"

-- | Convert an 'Int' to a 'D10' if it is within the range
-- 0 to 9, or 'Left' with an error message otherwise.
--
-- >>> intD10Either 5
-- Right [d10|5|]
--
-- >>> intD10Either 12
-- Left "d10 must be between 0 and 9"
--
-- >>> intD10Either (-5)
-- Left "d10 must be between 0 and 9"

intD10Either :: Num a => Int -> Either String (D10 a)
intD10Either x =
    case (intD10Maybe x) of
        Just y  ->  Right y
        Nothing ->  Left "d10 must be between 0 and 9"

-- | Convert a number of a type that has an 'Integral' instance
-- to a 'D10' if it falls within the range 0 to 9, or 'Left'
-- with an error message otherwise.
--
-- >>> integralD10Either (5 :: Integer)
-- Right [d10|5|]
--
-- >>> integralD10Either (12 :: Integer)
-- Left "d10 must be between 0 and 9"
--
-- >>> integralD10Either ((-5) :: Integer)
-- Left "d10 must be between 0 and 9"

integralD10Either :: (Num b, Integral a) => a -> Either String (D10 b)
integralD10Either x = integerD10Either (toInteger x)

---------------------------------------------------

-- | Convert a 'Char' to a 'D10' if it is within the range
-- @'0'@ to @'9'@, or 'fail' with an error message otherwise.
--
-- 'charD10Maybe' is a specialized version of this function.
--
-- >>> charD10Fail '5' :: IO (D10 Int)
-- [d10|5|]
--
-- >>> charD10Fail 'a' :: IO (D10 Int)
-- *** Exception: user error (d10 must be between 0 and 9)

charD10Fail :: (Num a, MonadFail m) => Char -> m (D10 a)
charD10Fail x
        | isD10Char x  =  return (D10_Unsafe (fromIntegral (ord x P.- ord '0')))
        | otherwise    =  fail "d10 must be between 0 and 9"

-- | Convert a 'String' to a 'D10' if it consists of a single
-- character and that character is within the range @'0'@ to
-- @'9'@, or 'fail' with an error message otherwise.
--
-- 'strD10Maybe' is a specialized version of this function.
--
-- >>> strD10Fail "5" :: IO (D10 Int)
-- [d10|5|]
--
-- >>> strD10Fail "a" :: IO (D10 Int)
-- *** Exception: user error (d10 must be between 0 and 9)
--
-- >>> strD10Fail "58" :: IO (D10 Int)
-- *** Exception: user error (d10 must be a single character)

strD10Fail :: (Num a, MonadFail m) => String -> m (D10 a)
strD10Fail [x]         =  charD10Fail x
strD10Fail _           =  fail "d10 must be a single character"

-- | Convert a 'String' to a 'D10' if all of the characters in
-- the string fall within the range @'0'@ to @'9'@, or 'fail'
-- with an error message otherwise.
--
-- 'strD10ListMaybe' is a specialized version of this function.
--
-- >>> strD10ListFail "5" :: IO [D10 Int]
-- [d10list|5|]
--
-- >>> strD10ListFail "a" :: IO [D10 Int]
-- *** Exception: user error (d10 must be between 0 and 9)
--
-- >>> strD10ListFail "58" :: IO [D10 Int]
-- [d10list|58|]

strD10ListFail :: (Num a, MonadFail m) => String -> m [D10 a]
strD10ListFail = traverse charD10Fail

-- | Convert a 'Natural' to a 'D10' if it is less than 10,
-- or 'fail' with an error message otherwise.
--
-- 'natD10Maybe' is a specialized version of this function.
--
-- 'integralD10Fail' is a more general version of this function.
--
-- >>> natD10Fail 5 :: IO (D10 Int)
-- [d10|5|]
--
-- >>> natD10Fail 12 :: IO (D10 Int)
-- *** Exception: user error (d10 must be less than 10)

natD10Fail :: (Num a, MonadFail m) => Natural -> m (D10 a)
natD10Fail x =
    case (natD10Maybe x) of
        Just y  -> return y
        Nothing -> fail "d10 must be less than 10"

-- | Convert an 'Integer' to a 'D10' if it is within the
-- range 0 to 9, or 'fail' with an error message otherwise.
--
-- 'integerD10Maybe' is a specialized version of this function.
--
-- 'integralD10Fail' is a more general version of this function.
--
-- >>> integerD10Fail 5 :: IO (D10 Int)
-- [d10|5|]
--
-- >>> integerD10Fail 12 :: IO (D10 Int)
-- *** Exception: user error (d10 must be between 0 and 9)
--
-- >>> integerD10Fail (-5) :: IO (D10 Int)
-- *** Exception: user error (d10 must be between 0 and 9)

integerD10Fail :: (Num a, MonadFail m) => Integer -> m (D10 a)
integerD10Fail x =
    case (integerD10Maybe x) of
        Just y  -> return y
        Nothing -> fail "d10 must be between 0 and 9"

-- | Convert an 'Int' to a 'D10' if it is within the range
-- 0 to 9, or 'fail' with an error message otherwise.
--
-- 'intD10Maybe' is a specialized version of this function.
--
-- 'integralD10Fail' is a more general version of this function.
--
-- >>> intD10Fail 5 :: IO (D10 Int)
-- [d10|5|]
--
-- >>> intD10Fail 12 :: IO (D10 Int)
-- *** Exception: user error (d10 must be between 0 and 9)
--
-- >>> intD10Fail (-5) :: IO (D10 Int)
-- *** Exception: user error (d10 must be between 0 and 9)

intD10Fail :: (Num a, MonadFail m) => Int -> m (D10 a)
intD10Fail x =
    case (intD10Maybe x) of
        Just y  ->  return y
        Nothing ->  fail "d10 must be between 0 and 9"

-- | Convert a number of a type that has an 'Integral' instance
-- to a 'D10' if it falls within the range 0 to 9, or 'fail'
-- with an error message otherwise.
--
-- 'natD10Maybe', 'integerD10Maybe', 'intD10Maybe',
-- 'integralD10Maybe', 'natD10Fail', 'integerD10Fail', and
-- 'intD10Fail' are all specialized versions of this function.
--
-- >>> integralD10Fail (5 :: Integer) :: IO (D10 Int)
-- [d10|5|]
--
-- >>> integralD10Fail (12 :: Integer) :: IO (D10 Int)
-- *** Exception: user error (d10 must be between 0 and 9)
--
-- >>> integralD10Fail ((-5) :: Integer) :: IO (D10 Int)
-- *** Exception: user error (d10 must be between 0 and 9)

integralD10Fail :: (Num b, Integral a, MonadFail m) => a -> m (D10 b)
integralD10Fail x = integerD10Fail (toInteger x)
