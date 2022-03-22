module Fallible where

import Control.Monad.Fail (MonadFail (fail))
import Prelude hiding (fail)

newtype Fallible a = Fallible (Either String a)
    deriving newtype (Functor, Applicative, Monad, Eq, Show)

instance MonadFail Fallible where
    fail = Fallible . Left
