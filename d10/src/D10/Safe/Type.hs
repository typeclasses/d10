{-# language Safe #-}

module D10.Safe.Type (D10 (..)) where

import Data.Data (Data)
import GHC.Generics (Generic)

-- | A whole number between /0/ and /9/
data D10
    = D0  -- ^ Zero
    | D1  -- ^ One
    | D2  -- ^ Two
    | D3  -- ^ Three
    | D4  -- ^ Four
    | D5  -- ^ Five
    | D6  -- ^ Six
    | D7  -- ^ Seven
    | D8  -- ^ Eight
    | D9  -- ^ Nine
    deriving (Bounded, Enum, Eq, Ord, Show, Data, Generic)
