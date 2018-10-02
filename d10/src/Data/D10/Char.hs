{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.D10.Char
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
    , isD10Char

    -- * Converting between D10 and String
    , d10Str
    , strD10Maybe
    , strD10Fail
    , isD10Str

    -- * Converting between [D10] and String
    , strD10ListMaybe
    , strD10ListFail
    , isD10ListStr

    -- * Converting between D10 and Natural
    , d10Nat
    , natD10Maybe
    , natD10Fail
    , isD10Nat
    , natMod10

    -- * Converting between D10 and Integer
    , d10Integer
    , integerD10Maybe
    , integerD10Fail
    , isD10Integer
    , integerMod10

    -- * Converting between D10 and Int
    , d10Int
    , intD10Maybe
    , intD10Fail
    , isD10Int
    , intMod10

    -- * Converting between D10 and general numeric types
    , d10Num
    , integralD10Maybe
    , integralD10Fail
    , isD10Integral
    , integralMod10

    ) where

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

newtype D10 = D10_Unsafe Char
    deriving Lift

---------------------------------------------------

-- | Shows base-10 digits using the quasiquoters defined in
-- "Data.D10.Char". A single digit is displayed using 'd10'.
-- A list of digits is displayed using 'd10list'.

instance Show D10 where
    showsPrec _ x = showString "[d10|"     . showsChar x . showString "|]"
    showList xs   = showString "[d10list|" . showsStr xs . showString "|]"

showsChar :: D10 -> ShowS
showsChar (D10_Unsafe x) = showChar x

showsStr :: [D10] -> ShowS
showsStr = appEndo . foldMap (Endo . showsChar)

---------------------------------------------------

-- | Convert a 'D10' to its underlying 'Char' representation.
--
-- >>> d10Char [d10|7|]
-- '7'

d10Char :: D10 -> Char
d10Char (D10_Unsafe c) = c

-- | Convert a 'D10' to a 'String'.
--
-- @'d10Str' x = ['d10Char' x]@
--
-- >>> d10Str [d10|7|]
-- "7"

d10Str :: D10 -> String
d10Str (D10_Unsafe c) = [c]

-- | Convert a 'D10' to a 'Natural'.
--
-- 'd10Num' is a more general version of this function.
--
-- >>> d10Nat [d10|7|]
-- 7

d10Nat :: D10 -> Natural
d10Nat (D10_Unsafe x) = fromIntegral (ord x - ord '0')

-- | Convert a 'D10' to an 'Integer'.
--
-- 'd10Num' is a more general version of this function.
--
-- >>> d10Integer [d10|7|]
-- 7

d10Integer :: D10 -> Integer
d10Integer (D10_Unsafe x) = toInteger (ord x - ord '0')

-- | Convert a 'D10' to an 'Int'.
--
-- 'd10Num' is a more general version of this function.
--
-- >>> d10Int [d10|7|]
-- 7

d10Int :: D10 -> Int
d10Int (D10_Unsafe x) = ord x - ord '0'

-- | Convert a 'D10' to any kind of number with a 'Num' instance.
--
-- Specialized versions of this function include 'd10Nat',
-- 'd10Integer', and 'd10Int'.
--
-- >>> d10Num [d10|7|] :: Integer
-- 7

d10Num :: Num a => D10 -> a
d10Num (D10_Unsafe x) = fromIntegral (ord x - ord '0')

---------------------------------------------------

-- | The 'D10' which is uniquely congruent modulo 10 to the given 'Natural'.
--
-- 'integralMod10' is a more general version of this function.

natMod10 :: Natural -> D10
natMod10 x = D10_Unsafe (chr (ord '0' + fromIntegral (x `mod` 10)))

-- | The 'D10' which is uniquely congruent modulo 10 to the given 'Integer'.
--
-- 'integralMod10' is a more general version of this function.

integerMod10 :: Integer -> D10
integerMod10 x = D10_Unsafe (chr (ord '0' + fromInteger (x `mod` 10)))

-- | The 'D10' which is uniquely congruent modulo 10 to the given 'Int'.
--
-- 'integralMod10' is a more general version of this function.

intMod10 :: Int -> D10
intMod10 x = D10_Unsafe (chr (ord '0' + (x `mod` 10)))

-- | The 'D10' which is uniquely congruent modulo 10 to the given number
-- (whose type must have an instance of the 'Integral' class).
--
-- Specialized versions of this function include 'natMod10',
-- 'integerMod10', and 'intMod10'.

integralMod10 :: Integral a => a -> D10
integralMod10 x = D10_Unsafe (chr (ord '0' + fromIntegral (x `mod` 10)))

---------------------------------------------------

-- | Convert a 'Char' to a 'D10' if it is within the range
-- @'0'@ to @'9'@, or produce 'Nothing' otherwise.
--
-- 'charD10Fail' is a more general version of this function.

charD10Maybe :: Char -> Maybe D10
charD10Maybe x
        | isD10Char x  =  Just (D10_Unsafe x)
        | otherwise    =  Nothing

-- | Convert a 'String' to a 'D10' if it consists of exactly one
-- character and that character is within the range @'0'@ to @'9'@,
-- or produce 'Nothing' otherwise.
--
-- 'strD10Fail' is a more general version of this function.

strD10Maybe :: String -> Maybe D10
strD10Maybe [x] = charD10Maybe x
strD10Maybe _   = Nothing

-- | Convert a 'String' to a list of 'D10' if all of the characters
-- in the string are within the range @'0'@ to @'9'@, or produce
-- 'Nothing' otherwise.
--
-- 'strD10ListFail' is a more general version of this function.

strD10ListMaybe :: String -> Maybe [D10]
strD10ListMaybe = traverse charD10Maybe

-- | Convert a 'Natural' to a 'D10' if it is less than 10,
-- or produce 'Nothing' otherwise.
--
-- 'integralD10Maybe', 'natD10Fail', and 'integralD10Fail'
-- are more general versions of this function.

natD10Maybe :: Natural -> Maybe D10
natD10Maybe x
        | isD10Nat x  =  Just (D10_Unsafe (chr (fromIntegral x + ord '0')))
        | otherwise   =  Nothing

-- | Convert an 'Integer' to a 'D10' if it is within the range 0 to 9,
-- or produce 'Nothing' otherwise.
--
-- 'integralD10Maybe', 'integerD10Fail', and 'integralD10Fail'
-- are more general versions of this function.

integerD10Maybe :: Integer -> Maybe D10
integerD10Maybe x
        | isD10Integer x  =  Just (D10_Unsafe (chr (fromInteger x + ord '0')))
        | otherwise       =  Nothing

-- | Convert an 'Int' to a 'D10' if it is within the range 0 to 9,
-- or produce 'Nothing' otherwise.
--
-- 'integralD10Maybe', 'intD10Fail', and 'integralD10Fail'
-- are more general versions of this function.

intD10Maybe :: Int -> Maybe D10
intD10Maybe x
        | isD10Int x  =  Just (D10_Unsafe (chr (x + ord '0')))
        | otherwise   =  Nothing

-- | Construct a 'D10' from any kind of number with an 'Integral'
-- instance, or produce 'Nothing' if the number falls outside the
-- range 0 to 9.
--
-- Specialized versions of this function include 'natD10Maybe',
-- 'integerD10Maybe', and 'intD10Maybe'.
--
-- 'integralD10Fail' is a more general version of this function.

integralD10Maybe :: Integral a => a -> Maybe D10
integralD10Maybe x = integerD10Maybe (toInteger x)

---------------------------------------------------

-- | Convert a 'Char' to a 'D10' if it is within the range
-- @'0'@ to @'9'@, or 'fail' with an error message otherwise.
--
-- 'charD10Maybe' is a specialized version of this function.

charD10Fail :: MonadFail m => Char -> m D10
charD10Fail x
        | isD10Char x  =  return (D10_Unsafe x)
        | otherwise    =  fail "d10 must be between 0 and 9"

-- | Convert a 'String' to a 'D10' if it consists of a single
-- character and that character is within the range @'0'@ to
-- @'9'@, or 'fail' with an error message otherwise.
--
-- 'strD10Maybe' is a specialized version of this function.

strD10Fail :: MonadFail m => String -> m D10
strD10Fail [x]
        | isD10Char x  =  return (D10_Unsafe x)
        | otherwise    =  fail "d10 must be between 0 and 9"
strD10Fail _           =  fail "d10 must be a single character"

-- | Convert a 'String' to a 'D10' if all of the characters in
-- the string fall within the range @'0'@ to @'9'@, or 'fail'
-- with an error message otherwise.
--
-- 'strD10ListMaybe' is a specialized version of this function.

strD10ListFail :: MonadFail m => String -> m [D10]
strD10ListFail = traverse charD10Fail

-- | Convert a 'Natural' to a 'D10' if it is less than 10,
-- or 'fail' with an error message otherwise.
--
-- 'natD10Maybe' is a specialized version of this function.
--
-- 'integralD10Fail' is a more general version of this function.

natD10Fail :: MonadFail m => Natural -> m D10
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

integerD10Fail :: MonadFail m => Integer -> m D10
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

intD10Fail :: MonadFail m => Int -> m D10
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

integralD10Fail :: (Integral a, MonadFail m) => a -> m D10
integralD10Fail x = integerD10Fail (toInteger x)

---------------------------------------------------

-- | Determines whether a 'Char' is in the range @'0'@ to @'9'@.
--
-- @'isD10Char' x = 'Data.Maybe.isJust' ('charD10Maybe' x)@

isD10Char :: Char -> Bool
isD10Char x = x >= '0' && x <= '9'

-- | Determines whether a 'String' consists of a single character
-- and that character is within the range @'0'@ to @'9'@.
--
-- @'isD10Str' x = 'Data.Maybe.isJust' ('strD10Maybe' x)@

isD10Str :: String -> Bool
isD10Str [x] = isD10Char x
isD10Str _   = False

-- | Determines whether a 'String' consists entirely of characters
-- that are within the range @'0'@ to @'9'@.
--
-- @'isD10Str' x = 'Data.Maybe.isJust' ('strD10ListMaybe' x)@

isD10ListStr :: String -> Bool
isD10ListStr = all isD10Char

-- | Determines whether a 'Natural' is in the range 0 to 9.
--
-- @'isD10Nat' x = 'Data.Maybe.isJust' ('natD10Maybe' x)@

isD10Nat :: Natural -> Bool
isD10Nat x = x <= 9

-- | Determines whether an 'Integer' is in the range 0 to 9.
--
-- @'isD10Integer' x = 'Data.Maybe.isJust' ('integerD10Maybe' x)@

isD10Integer :: Integer -> Bool
isD10Integer x = x >= 0 && x <= 9

-- | Determines whether an 'Int' is in the range 0 to 9.
--
-- @'isD10Int' x = 'Data.Maybe.isJust' ('intD10Maybe' x)@

isD10Int :: Int -> Bool
isD10Int x = x >= 0 && x <= 9

-- | Determines whether a number whose type has an 'Integral'
-- instance is in the range 0 to 9.
--
-- @'isD10Integral' x = 'Data.Maybe.isJust' ('integralD10Maybe' x)@

isD10Integral :: Integral a => a -> Bool
isD10Integral x = isD10Integer (toInteger x)

---------------------------------------------------

qq :: Lift a => (String -> Q a) -> QuasiQuoter
qq f = QuasiQuoter (f >=> lift) undefined undefined undefined

-- | A single base-10 digit.
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

d10 :: QuasiQuoter
d10 = qq strD10Fail

-- | A list of base-10 digits.
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

d10list :: QuasiQuoter
d10list = qq strD10ListFail
