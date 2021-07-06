{-# language Unsafe, GeneralizedNewtypeDeriving #-}

module D10.Char.Unsafe (D10 (D10_Unsafe)) where

import qualified D10.Predicate as Predicate

import Data.Char (chr, ord)
import Data.Monoid (Endo (..))

---------------------------------------------------

-- | A 'Char' value between @'0'@ and @'9'@
newtype D10 =
    D10_Unsafe Char
      -- ^ The constructor's name include the word "unsafe" as a reminder
      --   that you should generally avoid using it directly, because it
      --   allows constructing invalid 'D10' values.
    deriving (Eq, Ord)

---------------------------------------------------

instance Bounded D10
  where
    minBound = D10_Unsafe '0'
    maxBound = D10_Unsafe '9'

---------------------------------------------------

instance Enum D10
  where
    fromEnum :: D10 -> Int
    fromEnum (D10_Unsafe x) = ord x - ord '0'

    toEnum :: Int -> D10
    toEnum x | Predicate.isD10Int x = D10_Unsafe (chr (x + ord '0'))
             | otherwise = error "d10 must be between 0 and 9"

    enumFrom :: D10 -> [D10]
    enumFrom x = enumFromTo x maxBound

    enumFromThen :: D10 -> D10 -> [D10]
    enumFromThen x y = enumFromThenTo x y bound
      where
        bound | fromEnum y >= fromEnum x = maxBound
              | otherwise                = minBound

    succ :: D10 -> D10
    succ (D10_Unsafe '9') = error "D10 overflow"
    succ (D10_Unsafe x) = D10_Unsafe (succ x)

    pred :: D10 -> D10
    pred (D10_Unsafe '0') = error "D10 underflow"
    pred (D10_Unsafe x) = D10_Unsafe (pred x)

---------------------------------------------------

instance Show D10 where
    showsPrec _ x = showString "[d10|"     . showsChar x . showString "|]"
    showList xs   = showString "[d10list|" . showsStr xs . showString "|]"

showsChar :: D10 -> ShowS
showsChar (D10_Unsafe x) = showChar x

showsStr :: [D10] -> ShowS
showsStr = appEndo . foldMap (Endo . showsChar)
