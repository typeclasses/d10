import Prelude hiding (fail, (+), (-), (*))

import Data.D10.Num
import qualified Data.D10.Predicate as Predicate

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Applicative ((<|>))
import Control.Exception (try, SomeException)
import Control.Monad (when)
import Control.Monad.Fail (MonadFail (fail))
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.Word (Word64)
import Language.Haskell.TH (runQ, Q)
import System.Exit (exitFailure)

main :: IO ()
main =
  do
    ok <- checkParallel $$(discover)
    when (not ok) exitFailure

genD10 :: Integral a => Gen (D10 a)
genD10 = Gen.enumBounded

prop_bounds_examples :: Property
prop_bounds_examples = withTests 1 $ property $ do
    minBound === ([d10|0|] :: D10 Int)
    maxBound === ([d10|9|] :: D10 Int)

prop_enumeration_examples :: Property
prop_enumeration_examples = withTests 1 $ property $ do
    [ [d10|5|] .. ]           === ([d10list|56789|]      :: [D10 Integer])
    [ [d10|4|] .. [d10|7|] ]  === ([d10list|4567|]       :: [D10 Integer])
    [ [d10|5|], [d10|4|] .. ] === ([d10list|543210|]     :: [D10 Integer])
    [ [d10|1|], [d10|3|] .. ] === ([d10list|13579|]      :: [D10 Integer])
    [ minBound .. maxBound ]  === ([d10list|0123456789|] :: [D10 Integer])

prop_lift_examples :: Property
prop_lift_examples = withTests 1 $ property $ do
    d10Char    ([d10|7|] :: D10 Integer) === '7'
    d10Str     ([d10|7|] :: D10 Integer) === "7"
    d10Nat     ([d10|7|] :: D10 Integer) ===  7
    d10Integer ([d10|7|] :: D10 Integer) ===  7
    d10Int     ([d10|7|] :: D10 Integer) ===  7
    d10Num     ([d10|7|] :: D10 Integer) ===  (7 :: Integer)

prop_d10Str :: Property
prop_d10Str = property $ do
    x :: D10 Integer <- forAll genD10
    d10Str x === [d10Char x]

prop_mod_examples :: Property
prop_mod_examples = withTests 1 $ property $ do
    natMod10       56  === ([d10|6|] :: D10 Integer)
    integerMod10   56  === ([d10|6|] :: D10 Integer)
    integerMod10 (-56) === ([d10|4|] :: D10 Integer)
    intMod10       56  === ([d10|6|] :: D10 Integer)
    intMod10     (-56) === ([d10|4|] :: D10 Integer)

    integralMod10   (56  :: Integer) === ([d10|6|] :: D10 Integer)
    integralMod10 ((-56) :: Integer) === ([d10|4|] :: D10 Integer)

prop_d10Maybe_examples :: Property
prop_d10Maybe_examples = withTests 1 $ property $ do
    charD10Maybe '5' === (Just [d10|5|] :: Maybe (D10 Integer))
    charD10Maybe 'a' === (Nothing       :: Maybe (D10 Integer))
    strD10Maybe "5"  === (Just [d10|5|] :: Maybe (D10 Integer))
    strD10Maybe "a"  === (Nothing       :: Maybe (D10 Integer))
    strD10Maybe "58" === (Nothing       :: Maybe (D10 Integer))

prop_isJust_string :: Property
prop_isJust_string = property $ do
    x <- forAll $ Gen.string (Range.linear 0 5) (Gen.digit <|> Gen.alpha)
    Predicate.isD10Str     x === isJust (strD10Maybe x     :: Maybe (D10 Integer))
    Predicate.isD10ListStr x === isJust (strD10ListMaybe x :: Maybe [D10 Integer])

prop_isJust_nat :: Property
prop_isJust_nat = property $ do
    x <- forAll $ Gen.integral (Range.linear 0 20)
    Predicate.isD10Nat x === isJust (natD10Maybe x :: Maybe (D10 Integer))

prop_d10ListMaybe_examples :: Property
prop_d10ListMaybe_examples = withTests 1 $ property $ do
    strD10ListMaybe "5"  === (Just [d10list|5|]  :: Maybe [D10 Integer])
    strD10ListMaybe "a"  === (Nothing            :: Maybe [D10 Integer])
    strD10ListMaybe "58" === (Just [d10list|58|] :: Maybe [D10 Integer])

prop_natD10Maybe_examples :: Property
prop_natD10Maybe_examples = withTests 1 $ property $ do
    natD10Maybe  5 === (Just [d10|5|] :: Maybe (D10 Integer))
    natD10Maybe 12 === (Nothing       :: Maybe (D10 Integer))

prop_isJust_integer :: Property
prop_isJust_integer = property $ do
    x <- forAll $ Gen.integral (Range.linearFrom 0 (-5) 15)
    Predicate.isD10Integer x === isJust (integerD10Maybe x :: Maybe (D10 Integer))

prop_integerD10Maybe_examples :: Property
prop_integerD10Maybe_examples = withTests 1 $ property $ do
    integerD10Maybe   5  === (Just [d10|5|] :: Maybe (D10 Integer))
    integerD10Maybe  12  === (Nothing       :: Maybe (D10 Integer))
    integerD10Maybe (-5) === (Nothing       :: Maybe (D10 Integer))

prop_isJust_int :: Property
prop_isJust_int = withTests 1 $ property $ do
    x <- forAll $ Gen.integral (Range.linearFrom 0 (-5) 15)
    Predicate.isD10Int x === isJust (intD10Maybe x :: Maybe (D10 Integer))

prop_intD10Maybe_examples :: Property
prop_intD10Maybe_examples = withTests 1 $ property $ do
    intD10Maybe   5  === (Just [d10|5|] :: Maybe (D10 Integer))
    intD10Maybe  12  === (Nothing       :: Maybe (D10 Integer))
    intD10Maybe (-5) === (Nothing       :: Maybe (D10 Integer))

prop_isJust_integral :: Property
prop_isJust_integral = property $ do
    x <- forAll $ Gen.integral (Range.linearFrom 0 (-5) 15)
    Predicate.isD10Integral (x :: Integer) === isJust (integerD10Maybe x :: Maybe (D10 Integer))

prop_integralD10Maybe_examples :: Property
prop_integralD10Maybe_examples = withTests 1 $ property $ do
    integralD10Maybe (   5  :: Integer ) === (Just [d10|5|] :: Maybe (D10 Integer))
    integralD10Maybe (  12  :: Integer ) === (Nothing       :: Maybe (D10 Integer))
    integralD10Maybe ( (-5) :: Integer ) === (Nothing       :: Maybe (D10 Integer))

prop_charD10Either_examples :: Property
prop_charD10Either_examples = withTests 1 $ property $ do
    charD10Either '5' === (Right [d10|5|]                     :: Either String (D10 Integer) )
    charD10Either 'a' === (Left "d10 must be between 0 and 9" :: Either String (D10 Integer) )

prop_strD10Either_examples :: Property
prop_strD10Either_examples = withTests 1 $ property $ do
    strD10Either  "5" === (Right [d10|5|]                        :: Either String (D10 Integer))
    strD10Either  "a" === (Left "d10 must be between 0 and 9"    :: Either String (D10 Integer))
    strD10Either "58" === (Left "d10 must be a single character" :: Either String (D10 Integer))

prop_strD10ListEither_examples :: Property
prop_strD10ListEither_examples = withTests 1 $ property $ do
    strD10ListEither  "5" === (Right [d10list|5|]                 :: Either String [D10 Integer])
    strD10ListEither  "a" === (Left "d10 must be between 0 and 9" :: Either String [D10 Integer])
    strD10ListEither "58" === (Right [d10list|58|]                :: Either String [D10 Integer])

prop_natD10ListEither_examples :: Property
prop_natD10ListEither_examples = withTests 1 $ property $ do
    natD10Either  5 === (Right [d10|5|]                  :: Either String (D10 Integer))
    natD10Either 12 === (Left "d10 must be less than 10" :: Either String (D10 Integer))

prop_integerD10ListEither_examples :: Property
prop_integerD10ListEither_examples = withTests 1 $ property $ do
    integerD10Either   5  === (Right [d10|5|]                     :: Either String (D10 Integer))
    integerD10Either  12  === (Left "d10 must be between 0 and 9" :: Either String (D10 Integer))
    integerD10Either (-5) === (Left "d10 must be between 0 and 9" :: Either String (D10 Integer))

prop_intD10ListEither_examples :: Property
prop_intD10ListEither_examples = withTests 1 $ property $ do
    intD10Either   5  === (Right [d10|5|]                     :: Either String (D10 Integer))
    intD10Either  12  === (Left "d10 must be between 0 and 9" :: Either String (D10 Integer))
    intD10Either (-5) === (Left "d10 must be between 0 and 9" :: Either String (D10 Integer))

prop_integralD10ListEither_examples :: Property
prop_integralD10ListEither_examples = withTests 1 $ property $ do
    integralD10Either   (5  :: Integer) === (Right [d10|5|]                     :: Either String (D10 Integer))
    integralD10Either  (12  :: Integer) === (Left "d10 must be between 0 and 9" :: Either String (D10 Integer))
    integralD10Either ((-5) :: Integer) === (Left "d10 must be between 0 and 9" :: Either String (D10 Integer))

newtype Fallible a = Fallible (Either String a)
    deriving newtype (Functor, Applicative, Monad, Eq, Show)

instance MonadFail Fallible where
    fail = Fallible . Left

prop_charD10Fail_examples :: Property
prop_charD10Fail_examples = withTests 1 $ property $ do
    (charD10Fail '5' :: Fallible (D10 Int)) === return [d10|5|]
    (charD10Fail 'a' :: Fallible (D10 Int)) === fail "d10 must be between 0 and 9"

prop_strD10Fail_examples :: Property
prop_strD10Fail_examples = withTests 1 $ property $ do
    (strD10Fail  "5" :: Fallible (D10 Int)) === return [d10|5|]
    (strD10Fail  "a" :: Fallible (D10 Int)) === fail "d10 must be between 0 and 9"
    (strD10Fail "58" :: Fallible (D10 Int)) === fail "d10 must be a single character"

prop_strD10ListFail_examples :: Property
prop_strD10ListFail_examples = withTests 1 $ property $ do
    (strD10ListFail  "5" :: Fallible [D10 Int]) === return [d10list|5|]
    (strD10ListFail  "a" :: Fallible [D10 Int]) === fail "d10 must be between 0 and 9"
    (strD10ListFail "58" :: Fallible [D10 Int]) === return [d10list|58|]

prop_natD10Fail_examples :: Property
prop_natD10Fail_examples = withTests 1 $ property $ do
    (natD10Fail  5 :: Fallible (D10 Int)) === return [d10|5|]
    (natD10Fail 12 :: Fallible (D10 Int)) === fail "d10 must be less than 10"

prop_integerD10Fail_examples :: Property
prop_integerD10Fail_examples = withTests 1 $ property $ do
    (integerD10Fail   5  :: Fallible (D10 Int)) === return [d10|5|]
    (integerD10Fail  12  :: Fallible (D10 Int)) === fail "d10 must be between 0 and 9"
    (integerD10Fail (-5) :: Fallible (D10 Int)) === fail "d10 must be between 0 and 9"

prop_intD10Fail_examples :: Property
prop_intD10Fail_examples = withTests 1 $ property $ do
    (intD10Fail   5  :: Fallible (D10 Int)) === return [d10|5|]
    (intD10Fail  12  :: Fallible (D10 Int)) === fail "d10 must be between 0 and 9"
    (intD10Fail (-5) :: Fallible (D10 Int)) === fail "d10 must be between 0 and 9"

prop_integralD10Fail_examples :: Property
prop_integralD10Fail_examples = withTests 1 $ property $ do
    (integralD10Fail   (5  :: Integer) :: Fallible (D10 Int)) === return [d10|5|]
    (integralD10Fail  (12  :: Integer) :: Fallible (D10 Int)) === fail "d10 must be between 0 and 9"
    (integralD10Fail ((-5) :: Integer) :: Fallible (D10 Int)) === fail "d10 must be between 0 and 9"

qFails :: Show a => Q a -> PropertyT IO ()
qFails (q :: Q a) =
  do
    result <- liftIO (try (runQ q) :: IO (Either SomeException a))
    case result of
        Left _ -> success
        Right _ -> failure

prop_spliceExp_examples :: Property
prop_spliceExp_examples = withTests 1 $ property $ do
    d10Nat ($(d10Exp 5) :: D10 Word64) === 5
    qFails (d10Exp 12)
    map d10Nat ($(d10ListExp "")   :: [D10 Word]) === []
    map d10Nat ($(d10ListExp "5")  :: [D10 Word]) === [5]
    map d10Nat ($(d10ListExp "58") :: [D10 Word]) === [5,8]

prop_splicePat_examples :: Property
prop_splicePat_examples = withTests 1 $ property $ do
    "B" === case (charD10Maybe '5') :: Maybe (D10 Int) of
        Just $(d10Pat 4) -> "A"
        Just $(d10Pat 5) -> "B"
        _                -> "C"
    qFails (d10Pat 12)
    qFails (d10ListPat "ab")

prop_qqPat_examples :: Property
prop_qqPat_examples = withTests 1 $ property $ do
    "B" === case charD10Maybe '5' :: Maybe (D10 Int) of
        Just [d10|4|] -> "A"
        Just [d10|5|] -> "B"
        _             -> "C"
    "B" === case charD10Maybe '5' :: Maybe (D10 Integer) of
        Just [d10|4|] -> "A"
        Just [d10|5|] -> "B"
        _             -> "C"
    "B" === case [d10list|56|] :: [D10 Int32] of
        [d10list|41|] -> "A"
        [d10list|56|] -> "B"
        _             -> "C"

prop_arithmetic_examples :: Property
prop_arithmetic_examples = withTests 1 $ property $ do
    [d10|2|] + [d10|3|] === ([d10|5|] :: D10 Int32)
    [d10|6|] + [d10|7|] === ([d10|3|] :: D10 Int32)
    [d10|7|] - [d10|5|] === ([d10|2|] :: D10 Int32)
    [d10|3|] - [d10|7|] === ([d10|6|] :: D10 Int32)
    [d10|2|] * [d10|4|] === ([d10|8|] :: D10 Int32)
    [d10|7|] * [d10|8|] === ([d10|6|] :: D10 Int32)
