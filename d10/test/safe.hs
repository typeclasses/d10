import Prelude hiding (fail, (+), (-), (*))

import D10.Safe
import D10.Safe.Arithmetic
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
    minBound === D0
    maxBound === D9

prop_enumeration_examples :: Property
prop_enumeration_examples = withTests 1 $ property $ do
    [ D5 .. ] === [D5,D6,D7,D8,D9]
    [ D4 .. D7 ] === [D4,D5,D6,D7]
    [ D5, D4 .. ] === [D5,D4,D3,D2,D1,D0]
    [ D1, D3 .. ] === [D1,D3,D5,D7,D9]
    [ minBound .. maxBound ] === [D0,D1,D2,D3,D4,D5,D6,D7,D8,D9]

prop_d10Char_examples :: Property
prop_d10Char_examples = withTests 1 $ property $ do
    d10Char D7 === '7'

prop_d10Str :: Property
prop_d10Str = property $ do
    x <- forAll genD10
    d10Str x === [d10Char x]

prop_d10Str_examples :: Property
prop_d10Str_examples = withTests 1 $ property $ do
    d10Str D7 === "7"

prop_d10Nat_examples :: Property
prop_d10Nat_examples = withTests 1 $ property $ do
    d10Nat D7 === 7

prop_d10Integer_examples :: Property
prop_d10Integer_examples = withTests 1 $ property $ do
    d10Integer D7 === 7

prop_d10Int_examples :: Property
prop_d10Int_examples = withTests 1 $ property $ do
    d10Int D7 === 7

prop_d10Num_examples :: Property
prop_d10Num_examples = withTests 1 $ property $ do
    d10Num D7 === (7 :: Integer)

prop_natMod10_examples :: Property
prop_natMod10_examples = withTests 1 $ property $ do
    natMod10 56 === D6

prop_integerMod10_examples :: Property
prop_integerMod10_examples = withTests 1 $ property $ do
    integerMod10 56 === D6
    integerMod10 (-56) === D4

prop_intMod10_examples :: Property
prop_intMod10_examples = withTests 1 $ property $ do
    intMod10 56 === D6
    intMod10 (-56) === D4

prop_integralMod10_examples :: Property
prop_integralMod10_examples = withTests 1 $ property $ do
    integralMod10 (56 :: Integer) === D6
    integralMod10 ((-56) :: Integer) === D4

prop_charD10Maybe :: Property
prop_charD10Maybe = property $ do
    x <- forAll (Gen.digit <|> Gen.alpha)
    Predicate.isD10Char x === isJust (charD10Maybe x)

prop_charD10Maybe_examples :: Property
prop_charD10Maybe_examples = withTests 1 $ property $ do
    charD10Maybe '5' === Just D5
    charD10Maybe 'a' === Nothing

prop_strD10Maybe :: Property
prop_strD10Maybe = property $ do
    x <- forAll $ Gen.string (Range.linear 0 5) (Gen.digit <|> Gen.alpha)
    Predicate.isD10Str x === isJust (strD10Maybe x)

prop_strD10Maybe_examples :: Property
prop_strD10Maybe_examples = withTests 1 $ property $ do
    strD10Maybe "5" === Just D5
    strD10Maybe "a" === Nothing
    strD10Maybe "58" === Nothing

prop_strD10ListMaybe :: Property
prop_strD10ListMaybe = property $ do
    x <- forAll $ Gen.string (Range.linear 0 5) (Gen.digit <|> Gen.alpha)
    Predicate.isD10ListStr x === isJust (strD10ListMaybe x)

prop_strD10ListMaybe_examples :: Property
prop_strD10ListMaybe_examples = withTests 1 $ property $ do
    strD10ListMaybe "5" === Just [D5]
    strD10ListMaybe "a" === Nothing
    strD10ListMaybe "58" === Just [D5,D8]

prop_natD10Maybe :: Property
prop_natD10Maybe = property $ do
    x <- forAll $ Gen.integral (Range.linear 0 20)
    Predicate.isD10Nat x === isJust (natD10Maybe x)

prop_natD10Maybe_examples :: Property
prop_natD10Maybe_examples = withTests 1 $ property $ do
    natD10Maybe 5 === Just D5
    natD10Maybe 12 === Nothing

prop_integerD10Maybe :: Property
prop_integerD10Maybe = property $ do
    x <- forAll $ Gen.integral (Range.linearFrom 0 (-5) 15)
    Predicate.isD10Integer x === isJust (integerD10Maybe x)

prop_integerD10Maybe_examples :: Property
prop_integerD10Maybe_examples = withTests 1 $ property $ do
    integerD10Maybe 5 === Just D5
    integerD10Maybe 12 === Nothing
    integerD10Maybe (-5) === Nothing

prop_intD10Maybe :: Property
prop_intD10Maybe = property $ do
    x :: Int <- forAll $ Gen.integral (Range.linearFrom 0 (-5) 15)
    Predicate.isD10Int x === isJust (intD10Maybe x)

prop_intD10Maybe_examples :: Property
prop_intD10Maybe_examples = withTests 1 $ property $ do
    intD10Maybe 5 === Just D5
    intD10Maybe 12 === Nothing
    intD10Maybe (-5) === Nothing

prop_integralD10Maybe :: Property
prop_integralD10Maybe = property $ do
    x :: Integer <- forAll $ Gen.integral (Range.linearFrom 0 (-5) 15)
    Predicate.isD10Integer x === isJust (integralD10Maybe x)

prop_integralD10Maybe_examples :: Property
prop_integralD10Maybe_examples = withTests 1 $ property $ do
    integralD10Maybe (5 :: Integer) === Just D5
    integralD10Maybe (12 :: Integer) === Nothing
    integralD10Maybe ((-5) :: Integer) === Nothing

prop_charD10Either_examples :: Property
prop_charD10Either_examples = withTests 1 $ property $ do
    charD10Either '5' === Right D5
    charD10Either 'a' === Left "d10 must be between 0 and 9"

prop_strD10Either_examples :: Property
prop_strD10Either_examples = withTests 1 $ property $ do
    strD10Either "5" === Right D5
    strD10Either "a" === Left "d10 must be between 0 and 9"
    strD10Either "58" === Left "d10 must be a single character"

prop_strD10ListEither_examples :: Property
prop_strD10ListEither_examples = withTests 1 $ property $ do
    strD10ListEither "5" === Right [D5]
    strD10ListEither "a" === Left "d10 must be between 0 and 9"
    strD10ListEither "58" === Right [D5,D8]

prop_natD10Either_examples :: Property
prop_natD10Either_examples = withTests 1 $ property $ do
    natD10Either 5 === Right D5
    natD10Either 12 === Left "d10 must be less than 10"

prop_integerD10Either_examples :: Property
prop_integerD10Either_examples = withTests 1 $ property $ do
    integerD10Either 5 === Right D5
    integerD10Either 12 === Left "d10 must be between 0 and 9"
    integerD10Either (-5) === Left "d10 must be between 0 and 9"

prop_intD10Either_examples :: Property
prop_intD10Either_examples = withTests 1 $ property $ do
    intD10Either 5 === Right D5
    intD10Either 12 === Left "d10 must be between 0 and 9"
    intD10Either (-5) === Left "d10 must be between 0 and 9"

prop_integralD10Either_examples :: Property
prop_integralD10Either_examples = withTests 1 $ property $ do
    integralD10Either (5 :: Integer) === Right D5
    integralD10Either (12 :: Integer) === Left "d10 must be between 0 and 9"
    integralD10Either (-5 :: Integer) === Left "d10 must be between 0 and 9"

prop_charD10Fail_examples :: Property
prop_charD10Fail_examples = withTests 1 $ property $ do
    (charD10Fail '5' :: Fallible D10) === return D5
    (charD10Fail 'a' :: Fallible D10) === fail "d10 must be between 0 and 9"

prop_strD10Fail_examples :: Property
prop_strD10Fail_examples = withTests 1 $ property $ do
    (strD10Fail "5" :: Fallible D10) === return D5
    (strD10Fail "a" :: Fallible D10) === fail "d10 must be between 0 and 9"
    (strD10Fail "58" :: Fallible D10) === fail "d10 must be a single character"

prop_strD10ListFail_examples :: Property
prop_strD10ListFail_examples = withTests 1 $ property $ do
    (strD10ListFail "5" :: Fallible [D10]) === return [D5]
    (strD10ListFail "a" :: Fallible [D10]) === fail "d10 must be between 0 and 9"
    (strD10ListFail "58" :: Fallible [D10]) === return [D5,D8]

prop_natD10Fail_examples :: Property
prop_natD10Fail_examples = withTests 1 $ property $ do
    (natD10Fail 5 :: Fallible D10) === return D5
    (natD10Fail 12 :: Fallible D10) === fail "d10 must be less than 10"

prop_integerD10Fail_examples :: Property
prop_integerD10Fail_examples = withTests 1 $ property $ do
    (integerD10Fail 5 :: Fallible D10) === return D5
    (integerD10Fail 12 :: Fallible D10) === fail "d10 must be between 0 and 9"
    (integerD10Fail (-5) :: Fallible D10) === fail "d10 must be between 0 and 9"

prop_intD10Fail_examples :: Property
prop_intD10Fail_examples = withTests 1 $ property $ do
    (intD10Fail 5 :: Fallible D10) === return D5
    (intD10Fail 12 :: Fallible D10) === fail "d10 must be between 0 and 9"
    (intD10Fail (-5) :: Fallible D10) === fail "d10 must be between 0 and 9"

prop_integralD10Fail_examples :: Property
prop_integralD10Fail_examples = withTests 1 $ property $ do
    (integralD10Fail (5 :: Integer) :: Fallible D10) === return D5
    (integralD10Fail (12 :: Integer) :: Fallible D10) === fail "d10 must be between 0 and 9"
    (integralD10Fail (-5 :: Integer) :: Fallible D10) === fail "d10 must be between 0 and 9"

prop_d10ListExp_examples :: Property
prop_d10ListExp_examples = withTests 1 $ property $ do
    $(d10ListExp "") === ([] :: [D10])
    $(d10ListExp "5") === [D5]
    $(d10ListExp "58") === [D5,D8]
    qFails (d10ListExp "a")

prop_d10ListPat_examples :: Property
prop_d10ListPat_examples = withTests 1 $ property $ do
    "B" === case id [D5, D6] of
        $(d10ListPat "42") -> "A"
        $(d10ListPat "56") -> "B"
        _                  -> "C"
    qFails (d10ListPat "a")

prop_qqExp_examples :: Property
prop_qqExp_examples = withTests 1 $ property $ do
    [d10list||] === ([] :: [D10])
    [d10list|5|] === [D5]
    [d10list|58|] === [D5,D8]

prop_qqPat_examples :: Property
prop_qqPat_examples = withTests 1 $ property $ do
    "B" === case id [D5, D6] of
        [d10list|41|] -> "A"
        [d10list|56|] -> "B"
        _             -> "C"

prop_arithmetic_examples :: Property
prop_arithmetic_examples = withTests 1 $ property $ do
    D0 + D3 === D3
    D3 + D6 === D9
    D2 + D3 === D5
    D6 + D7 === D3
    D7 - D5 === D2
    D3 - D7 === D6
    D2 * D4 === D8
    D7 * D8 === D6
