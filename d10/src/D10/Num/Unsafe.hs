{-# language Unsafe, GeneralizedNewtypeDeriving #-}

module D10.Num.Unsafe (D10 (..)) where

import qualified D10.Predicate as Predicate

import Data.Char (chr, ord)
import Data.Monoid (Endo (..))
import Language.Haskell.TH.Syntax (Lift)

---------------------------------------------------

-- | A value of some numeric type @a@ between
-- @'fromInteger' 0@ and @'fromInteger' 9@.
newtype D10 a =
    D10_Unsafe a
      -- ^ The constructor's name include the word "unsafe" as a reminder
      --   that you should generally avoid using it directly, because it
      --   allows constructing invalid 'D10' values.
    deriving (Eq, Ord, Lift)

---------------------------------------------------

instance Num a => Bounded (D10 a)
  where
    minBound = D10_Unsafe 0
    maxBound = D10_Unsafe 9

---------------------------------------------------

instance Integral a => Enum (D10 a)
  where
    fromEnum :: D10 a -> Int
    fromEnum (D10_Unsafe x) = fromIntegral x

    toEnum :: Int -> D10 a
    toEnum x | Predicate.isD10Int x = D10_Unsafe (fromIntegral x)
             | otherwise = error "d10 must be between 0 and 9"

    enumFrom :: D10 a -> [D10 a]
    enumFrom x = enumFromTo x maxBound

    enumFromThen :: D10 a -> D10 a -> [D10 a]
    enumFromThen x y = enumFromThenTo x y bound
      where
        bound | fromEnum y >= fromEnum x = maxBound
              | otherwise                = minBound

    succ (D10_Unsafe 9) = error "D10 overflow"
    succ (D10_Unsafe x) = D10_Unsafe (succ x)

    pred (D10_Unsafe 0) = error "D10 underflow"
    pred (D10_Unsafe x) = D10_Unsafe (pred x)

---------------------------------------------------

instance Integral a => Show (D10 a) where
    showsPrec _ x = showString "[d10|"     . showsChar x . showString "|]"
    showList xs   = showString "[d10list|" . showsStr xs . showString "|]"

showsChar :: Integral a => D10 a -> ShowS
showsChar (D10_Unsafe x) = showChar $ chr (ord '0' + fromIntegral x)

showsStr :: Integral a => [D10 a] -> ShowS
showsStr = appEndo . foldMap (Endo . showsChar)
