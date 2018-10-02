{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

-- | Defines a 'D10' type as a newtype for any type with an
-- instance of the 'Num' class, where the values are restricted
-- to numbers between @'fromInteger' 0@ and @'fromInteger' 9@.

module Data.D10.Num
    (
    -- * Type
      D10 (..)

    -- * Quasi-quoters
    , d10
    , d10list

    -- * Converting between D10 and Char
    , d10Char
    , charD10Maybe
    , charD10Fail

    -- * Converting between D10 and String
    , d10Str
    , strD10Maybe
    , strD10Fail
    , isD10Str

    -- * Converting between [D10] and String
    , strD10ListMaybe
    , strD10ListFail

    -- * Converting between D10 and Natural
    , d10Nat
    , natD10Maybe
    , natD10Fail
    , natMod10

    -- * Converting between D10 and Integer
    , d10Integer
    , integerD10Maybe
    , integerD10Fail
    , integerMod10

    -- * Converting between D10 and Int
    , d10Int
    , intD10Maybe
    , intD10Fail
    , intMod10

    -- * Converting between D10 and general numeric types
    , d10Num
    , integralD10Maybe
    , integralD10Fail
    , integralMod10

    ) where

import Data.D10.Predicate

-- base
import           Control.Monad              ((>=>))
import           Control.Monad.Fail         (MonadFail (fail))
import           Data.Char                  (chr, ord)
import           Data.Monoid                (Endo (..))
import           GHC.Generics               (Generic)
import           Numeric.Natural            (Natural)
import           Prelude                    hiding (fail)

-- template-haskell
import           Language.Haskell.TH        (ExpQ, Q)
import           Language.Haskell.TH.Quote  (QuasiQuoter (QuasiQuoter))
import           Language.Haskell.TH.Syntax (Lift (lift))

-- $setup
-- >>> :set -XQuasiQuotes

---------------------------------------------------

-- | A value of some numeric type @a@ between
-- @'fromInteger' 0@ and @'fromInteger' 9@.
--
-- The "Data.D10.Num" module provides many functions for
-- constructing 'D10' values, including:
--
-- * @'integerD10Maybe' :: 'Integer' -> 'Maybe' 'D10'@
-- * @'integerMod10' :: 'Integer' -> 'D10'@
--
-- With the @QuasiQuotes@ GHC extension enabled, you can write
-- 'D10' literals using the quasi-quoters 'd10' and 'd10list'.

newtype D10 a =
    D10_Unsafe a
      -- ^ The constructor's name include the word "unsafe" as a reminder
      --   that you should generally avoid using it directly, because it
      --   allows constructing invalid 'D10' values.
    deriving (Eq, Ord, Lift)

instance Num a => Bounded (D10 a)
  where
    minBound = D10_Unsafe 0
    maxBound = D10_Unsafe 9

---------------------------------------------------

-- | Shows base-10 digits using the quasiquoters defined in
-- "Data.D10.Char". A single digit is displayed using 'd10'.
-- A list of digits is displayed using 'd10list'.

instance Integral a => Show (D10 a) where
    showsPrec _ x = showString "[d10|"     . showsChar x . showString "|]"
    showList xs   = showString "[d10list|" . showsStr xs . showString "|]"

showsChar :: Integral a => D10 a -> ShowS
showsChar = showChar . d10Char

showsStr :: Integral a => [D10 a] -> ShowS
showsStr = appEndo . foldMap (Endo . showsChar)

---------------------------------------------------

-- | Convert a 'D10' to its underlying 'Char' representation.
--
-- >>> d10Char [d10|7|]
-- '7'

d10Char :: Integral a => D10 a -> Char
d10Char (D10_Unsafe x) = chr (ord '0' + fromIntegral x)

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
-- @'Data.D10.Predicate.isD10Char' x = 'Data.Maybe.isJust' ('charD10Maybe' x)@
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
        | isD10Char x  =  Just (D10_Unsafe (fromIntegral (ord x - ord '0')))
        | otherwise    =  Nothing

-- | Convert a 'String' to a 'D10' if it consists of exactly one
-- character and that character is within the range @'0'@ to @'9'@,
-- or produce 'Nothing' otherwise.
--
-- @'Data.D10.Predicate.isD10Str' x = 'Data.Maybe.isJust' ('strD10Maybe' x)@
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
-- @'Data.D10.Predicate.isD10Str' x = 'Data.Maybe.isJust' ('strD10ListMaybe' x)@
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
-- @'Data.D10.Predicate.isD10Nat' x = 'Data.Maybe.isJust' ('natD10Maybe' x)@
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
-- @'Data.D10.Predicate.isD10Integer' x = 'Data.Maybe.isJust' ('integerD10Maybe' x)@
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
-- @'Data.D10.Predicate.isD10Int' x = 'Data.Maybe.isJust' ('intD10Maybe' x)@
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
-- @'Data.D10.Predicate.isD10Integral' x = 'Data.Maybe.isJust' ('integralD10Maybe' x)@
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
        | isD10Char x  =  return (D10_Unsafe (fromIntegral (ord x - ord '0')))
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

---------------------------------------------------

qq :: Lift a => (String -> Q a) -> QuasiQuoter
qq f = QuasiQuoter (f >=> lift) undefined undefined undefined

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
-- ... • d10 must be between 0 and 9
-- ... • In the quasi-quotation: [d10|a|]
--
-- >>> d10Nat [d10|58|]
-- ...
-- ... • d10 must be a single character
-- ... • In the quasi-quotation: [d10|58|]

d10 :: forall a. (Lift a, Num a) => QuasiQuoter
d10 = qq @(D10 a) strD10Fail

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
-- ... • d10 must be between 0 and 9
-- ... • In the quasi-quotation: [d10list|a|]

d10list :: forall a. (Lift a, Num a) => QuasiQuoter
d10list = qq @[D10 a] strD10ListFail
