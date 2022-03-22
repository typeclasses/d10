module AssertQFails where

import Hedgehog

import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Language.Haskell.TH (runQ, Q)

qFails :: Show a => Q a -> PropertyT IO ()
qFails (q :: Q a) =
  do
    result <- liftIO (try (runQ q) :: IO (Either SomeException a))
    case result of
        Left _ -> success
        Right _ -> failure
