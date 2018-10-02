{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.D10.Char
    (
    -- * Type
      D10 (..)

    -- * Quasi-quoters
    , d10
    , d10s

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
    , strD10sFail
    , isD10sStr

    -- * Converting between D10 and Natural
    , d10Nat
    , natD10Maybe
    , natD10Fail
    , isD10Nat

    -- * Converting between D10 and Int
    , d10Int
    , intD10Maybe
    , intD10Fail
    , isD10Int

    -- * Converting between D10 and Integer
    , d10Integer
    , integerD10Maybe
    , integerD10Fail
    , isD10Integer

    -- * Converting between D10 and general numeric types
    , d10Num
    , integralD10Maybe
    , integralD10Fail
    , isD10Integral

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

---------------------------------------------------

newtype D10 = D10_Unsafe Char
    deriving Lift

instance Show D10 where
    showsPrec _ x = showString "[d10|"  . showsChar x . showString "|]"
    showList xs   = showString "[d10s|" . showsStr xs . showString "|]"

showsChar :: D10 -> ShowS
showsChar (D10_Unsafe x) = showChar x

showsStr :: [D10] -> ShowS
showsStr = appEndo . foldMap (Endo . showsChar)

---------------------------------------------------

d10Char :: D10 -> Char
d10Char (D10_Unsafe c) = c

d10Str :: D10 -> String
d10Str (D10_Unsafe c) = [c]

d10Nat :: D10 -> Natural
d10Nat (D10_Unsafe x) = fromIntegral (ord x - ord '0')

d10Int :: D10 -> Int
d10Int (D10_Unsafe x) = ord x - ord '0'

d10Integer :: D10 -> Integer
d10Integer (D10_Unsafe x) = toInteger (ord x - ord '0')

d10Num :: Num a => D10 -> a
d10Num (D10_Unsafe x) = fromIntegral (ord x - ord '0')

---------------------------------------------------

charD10Maybe :: Char -> Maybe D10
charD10Maybe x
        | isD10Char x  =  Just (D10_Unsafe x)
        | otherwise    =  Nothing

strD10Maybe :: String -> Maybe D10
strD10Maybe [x] = charD10Maybe x
strD10Maybe _   = Nothing

natD10Maybe :: Natural -> Maybe D10
natD10Maybe x
        | isD10Nat x  =  Just (D10_Unsafe (chr (fromIntegral x + ord '0')))
        | otherwise   =  Nothing

intD10Maybe :: Int -> Maybe D10
intD10Maybe x
        | isD10Int x  =  Just (D10_Unsafe (chr (x + ord '0')))
        | otherwise   =  Nothing

integerD10Maybe :: Integer -> Maybe D10
integerD10Maybe x
        | isD10Integer x  =  Just (D10_Unsafe (chr (fromInteger x + ord '0')))
        | otherwise       =  Nothing

integralD10Maybe :: Integral a => a -> Maybe D10
integralD10Maybe x = integerD10Maybe (toInteger x)

---------------------------------------------------

charD10Fail :: MonadFail m => Char -> m D10
charD10Fail x
        | isD10Char x  =  return (D10_Unsafe x)
        | otherwise    =  fail "d10 must be between 0 and 9"

strD10Fail :: MonadFail m => String -> m D10
strD10Fail [x]
        | isD10Char x  =  return (D10_Unsafe x)
        | otherwise    =  fail "d10 must be between 0 and 9"
strD10Fail _           =  fail "d10 must be a single character"

strD10sFail :: MonadFail m => String -> m [D10]
strD10sFail = traverse charD10Fail

natD10Fail :: MonadFail m => Natural -> m D10
natD10Fail x =
    case (natD10Maybe x) of
        Just y  -> return y
        Nothing -> fail "d10 must be less than 10"

intD10Fail :: MonadFail m => Int -> m D10
intD10Fail x =
    case (intD10Maybe x) of
        Just y  ->  return y
        Nothing ->  fail "d10 must be between 0 and 9"

integerD10Fail :: MonadFail m => Integer -> m D10
integerD10Fail x =
    case (integerD10Maybe x) of
        Just y  -> return y
        Nothing -> fail "d10 must be between 0 and 9"

integralD10Fail :: (Integral a, MonadFail m) => a -> m D10
integralD10Fail x = integerD10Fail (toInteger x)

---------------------------------------------------

isD10Char :: Char -> Bool
isD10Char x = x >= '0' && x <= '9'

isD10Str :: String -> Bool
isD10Str [x] = isD10Char x
isD10Str _   = False

isD10sStr :: String -> Bool
isD10sStr = all isD10Char

isD10Nat :: Natural -> Bool
isD10Nat x = x <= 9

isD10Int :: Int -> Bool
isD10Int x = x >= 0 && x <= 9

isD10Integer :: Integer -> Bool
isD10Integer x = x >= 0 && x <= 9

isD10Integral :: Integral a => a -> Bool
isD10Integral x = isD10Integer (toInteger x)

---------------------------------------------------

qq :: Lift a => (String -> Q a) -> QuasiQuoter
qq f = QuasiQuoter (f >=> lift) undefined undefined undefined

d10 :: QuasiQuoter
d10 = qq strD10Fail

d10s :: QuasiQuoter
d10s = qq strD10sFail
