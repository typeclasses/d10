import Prelude hiding (fail, (+), (-), (*))

import D10.Char
import qualified D10.Predicate as Predicate

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Fail (MonadFail (fail))
import Data.Maybe (isJust)
import System.Exit (exitFailure)

import AssertQFails (qFails)
import Fallible (Fallible)

main :: IO ()
main =
  do
    ok <- checkParallel $$(discover)
    when (not ok) exitFailure

genD10 :: Gen D10
genD10 = Gen.enumBounded

prop_bounds_examples :: Property
prop_bounds_examples = withTests 1 $ property $ do
    minBound === [d10|0|]
    maxBound === [d10|9|]

prop_enumeration_examples :: Property
prop_enumeration_examples = withTests 1 $ property $ do
    [ [d10|5|] .. ]           === [d10list|56789|]
    [ [d10|4|] .. [d10|7|] ]  === [d10list|4567|]
    [ [d10|5|], [d10|4|] .. ] === [d10list|543210|]
    [ [d10|1|], [d10|3|] .. ] === [d10list|13579|]
    [ minBound .. maxBound ]  === [d10list|0123456789|]

prop_lift_examples :: Property
prop_lift_examples = withTests 1 $ property $ do
    d10Char    [d10|7|] === '7'
    d10Str     [d10|7|] === "7"
    d10Nat     [d10|7|] ===  7
    d10Integer [d10|7|] ===  7
    d10Int     [d10|7|] ===  7
    d10Num     [d10|7|] ===  (7 :: Integer)

prop_d10Str :: Property
prop_d10Str = property $ do
    x <- forAll genD10
    d10Str x === [d10Char x]

prop_mod_examples :: Property
prop_mod_examples = withTests 1 $ property $ do
    natMod10       56  === [d10|6|]
    integerMod10   56  === [d10|6|]
    integerMod10 (-56) === [d10|4|]
    intMod10       56  === [d10|6|]
    intMod10     (-56) === [d10|4|]

    integralMod10   (56  :: Integer) === [d10|6|]
    integralMod10 ((-56) :: Integer) === [d10|4|]

prop_d10Maybe_examples :: Property
prop_d10Maybe_examples = withTests 1 $ property $ do
    charD10Maybe '5' === Just [d10|5|]
    charD10Maybe 'a' === Nothing
    strD10Maybe "5"  === Just [d10|5|]
    strD10Maybe "a"  === Nothing
    strD10Maybe "58" === Nothing

prop_isJust_string :: Property
prop_isJust_string = property $ do
    x <- forAll $ Gen.string (Range.linear 0 5) (Gen.digit <|> Gen.alpha)
    Predicate.isD10Str     x === isJust (strD10Maybe x)
    Predicate.isD10ListStr x === isJust (strD10ListMaybe x)

prop_isJust_nat :: Property
prop_isJust_nat = property $ do
    x <- forAll $ Gen.integral (Range.linear 0 20)
    Predicate.isD10Nat x === isJust (natD10Maybe x)

prop_d10ListMaybe_examples :: Property
prop_d10ListMaybe_examples = withTests 1 $ property $ do
    strD10ListMaybe "5"  === Just [d10list|5|]
    strD10ListMaybe "a"  === Nothing
    strD10ListMaybe "58" === Just [d10list|58|]

prop_natD10Maybe_examples :: Property
prop_natD10Maybe_examples = withTests 1 $ property $ do
    natD10Maybe  5 === Just [d10|5|]
    natD10Maybe 12 === Nothing

prop_isJust_integer :: Property
prop_isJust_integer = property $ do
    x <- forAll $ Gen.integral (Range.linearFrom 0 (-5) 15)
    Predicate.isD10Integer x === isJust (integerD10Maybe x)

prop_integerD10Maybe_examples :: Property
prop_integerD10Maybe_examples = withTests 1 $ property $ do
    integerD10Maybe   5  === Just [d10|5|]
    integerD10Maybe  12  === Nothing
    integerD10Maybe (-5) === Nothing

prop_isJust_int :: Property
prop_isJust_int = withTests 1 $ property $ do
    x <- forAll $ Gen.integral (Range.linearFrom 0 (-5) 15)
    Predicate.isD10Int x === isJust (intD10Maybe x)

prop_intD10Maybe_examples :: Property
prop_intD10Maybe_examples = withTests 1 $ property $ do
    intD10Maybe   5  === Just [d10|5|]
    intD10Maybe  12  === Nothing
    intD10Maybe (-5) === Nothing

prop_isJust_integral :: Property
prop_isJust_integral = property $ do
    x <- forAll $ Gen.integral (Range.linearFrom 0 (-5) 15)
    Predicate.isD10Integral (x :: Integer) === isJust (integerD10Maybe x)

prop_integralD10Maybe_examples :: Property
prop_integralD10Maybe_examples = withTests 1 $ property $ do
    integralD10Maybe (   5  :: Integer ) === Just [d10|5|]
    integralD10Maybe (  12  :: Integer ) === Nothing
    integralD10Maybe ( (-5) :: Integer ) === Nothing

prop_charD10Either_examples :: Property
prop_charD10Either_examples = withTests 1 $ property $ do
    charD10Either '5' === Right [d10|5|]
    charD10Either 'a' === Left "d10 must be between 0 and 9"

prop_strD10Either_examples :: Property
prop_strD10Either_examples = withTests 1 $ property $ do
    strD10Either  "5" === Right [d10|5|]
    strD10Either  "a" === Left "d10 must be between 0 and 9"
    strD10Either "58" === Left "d10 must be a single character"

prop_strD10ListEither_examples :: Property
prop_strD10ListEither_examples = withTests 1 $ property $ do
    strD10ListEither  "5" === Right [d10list|5|]
    strD10ListEither  "a" === Left "d10 must be between 0 and 9"
    strD10ListEither "58" === Right [d10list|58|]

prop_natD10ListEither_examples :: Property
prop_natD10ListEither_examples = withTests 1 $ property $ do
    natD10Either  5 === Right [d10|5|]
    natD10Either 12 === Left "d10 must be less than 10"

prop_integerD10ListEither_examples :: Property
prop_integerD10ListEither_examples = withTests 1 $ property $ do
    integerD10Either   5  === Right [d10|5|]
    integerD10Either  12  === Left "d10 must be between 0 and 9"
    integerD10Either (-5) === Left "d10 must be between 0 and 9"

prop_intD10ListEither_examples :: Property
prop_intD10ListEither_examples = withTests 1 $ property $ do
    intD10Either   5  === Right [d10|5|]
    intD10Either  12  === Left "d10 must be between 0 and 9"
    intD10Either (-5) === Left "d10 must be between 0 and 9"

prop_integralD10ListEither_examples :: Property
prop_integralD10ListEither_examples = withTests 1 $ property $ do
    integralD10Either   (5  :: Integer) === Right [d10|5|]
    integralD10Either  (12  :: Integer) === Left "d10 must be between 0 and 9"
    integralD10Either ((-5) :: Integer) === Left "d10 must be between 0 and 9"

prop_charD10Fail_examples :: Property
prop_charD10Fail_examples = withTests 1 $ property $ do
    (charD10Fail '5' :: Fallible D10) === return [d10|5|]
    (charD10Fail 'a' :: Fallible D10) === fail "d10 must be between 0 and 9"

prop_strD10Fail_examples :: Property
prop_strD10Fail_examples = withTests 1 $ property $ do
    (strD10Fail  "5" :: Fallible D10) === return [d10|5|]
    (strD10Fail  "a" :: Fallible D10) === fail "d10 must be between 0 and 9"
    (strD10Fail "58" :: Fallible D10) === fail "d10 must be a single character"

prop_strD10ListFail_examples :: Property
prop_strD10ListFail_examples = withTests 1 $ property $ do
    (strD10ListFail  "5" :: Fallible [D10]) === return [d10list|5|]
    (strD10ListFail  "a" :: Fallible [D10]) === fail "d10 must be between 0 and 9"
    (strD10ListFail "58" :: Fallible [D10]) === return [d10list|58|]

prop_natD10Fail_examples :: Property
prop_natD10Fail_examples = withTests 1 $ property $ do
    (natD10Fail  5 :: Fallible D10) === return [d10|5|]
    (natD10Fail 12 :: Fallible D10) === fail "d10 must be less than 10"

prop_integerD10Fail_examples :: Property
prop_integerD10Fail_examples = withTests 1 $ property $ do
    (integerD10Fail   5  :: Fallible D10) === return [d10|5|]
    (integerD10Fail  12  :: Fallible D10) === fail "d10 must be between 0 and 9"
    (integerD10Fail (-5) :: Fallible D10) === fail "d10 must be between 0 and 9"

prop_intD10Fail_examples :: Property
prop_intD10Fail_examples = withTests 1 $ property $ do
    (intD10Fail   5  :: Fallible D10) === return [d10|5|]
    (intD10Fail  12  :: Fallible D10) === fail "d10 must be between 0 and 9"
    (intD10Fail (-5) :: Fallible D10) === fail "d10 must be between 0 and 9"

prop_integralD10Fail_examples :: Property
prop_integralD10Fail_examples = withTests 1 $ property $ do
    (integralD10Fail   (5  :: Integer) :: Fallible D10) === return [d10|5|]
    (integralD10Fail  (12  :: Integer) :: Fallible D10) === fail "d10 must be between 0 and 9"
    (integralD10Fail ((-5) :: Integer) :: Fallible D10) === fail "d10 must be between 0 and 9"

prop_spliceExp_examples :: Property
prop_spliceExp_examples = withTests 1 $ property $ do
    d10Nat $(d10Exp 5) === 5
    qFails (d10Exp 12)
    (d10Nat <$> $(d10ListExp "")) === []
    (d10Nat <$> $(d10ListExp "5")) === [5]
    (d10Nat <$> $(d10ListExp "58")) === [5,8]

prop_splicePat_examples :: Property
prop_splicePat_examples = withTests 1 $ property $ do
    "B" === case (charD10Maybe '5') of
        Just $(d10Pat 4) -> "A"
        Just $(d10Pat 5) -> "B"
        _                -> "C"
    qFails (d10Pat 12)
    "B" === case (strD10ListMaybe "56") of
        Just $(d10ListPat "42") -> "A"
        Just $(d10ListPat "56") -> "B"
        _                       -> "C"

prop_qqPat_examples :: Property
prop_qqPat_examples = withTests 1 $ property $ do
    "B" === case (charD10Maybe '5') of
        Just [d10|4|] -> "A"
        Just [d10|5|] -> "B"
        _             -> "C"
    "B" === case [d10list|56|] of
        [d10list|41|] -> "A"
        [d10list|56|] -> "B"
        _             -> "C"

prop_arithmetic_examples :: Property
prop_arithmetic_examples = withTests 1 $ property $ do
    [d10|2|] + [d10|3|] === [d10|5|]
    [d10|6|] + [d10|7|] === [d10|3|]
    [d10|7|] - [d10|5|] === [d10|2|]
    [d10|3|] - [d10|7|] === [d10|6|]
    [d10|2|] * [d10|4|] === [d10|8|]
    [d10|7|] * [d10|8|] === [d10|6|]
