{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HuttonsRazorTest where

import           Hedgehog            (MonadGen, forAll, tripping, property)
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Test.Tasty          (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import           HuttonsRazor        (Razor (Add, IfThenElse, LitB, LitI, Or),
                                      parseText, pretty)

testHuttonsRazor :: TestTree
testHuttonsRazor = testGroup "HuttonsRazor"
  [
    testPrintParse
  ]

testPrintParse :: TestTree
testPrintParse = testProperty "parse . pretty" . property $ do
  r <- forAll genRazor
  tripping r pretty parseText

newtype Depth = Depth Int
  deriving (Eq, Ord, Enum)

genRazor, genLitI, genLitB, genAdd, genOr, genBool, genNumeric ::
  MonadGen m
  => m Razor

genRazor =
  let
    terminals = [genLitI, genLitB]
    recursive = [genITE genRazor genRazor, genAdd, genOr]
  in
    Gen.recursive Gen.choice terminals recursive


genBool =
  Gen.recursive Gen.choice [genLitB] [genITE genBool genBool, genOr]

genLitI =
  LitI <$> Gen.integral (Range.linear 0 1000000)

genLitB =
  LitB <$> Gen.bool

genITE ::
  MonadGen m
  => m Razor
  -> m Razor
  -> m Razor
genITE g1 g2 =
  Gen.subterm3 genBool g1 g2 IfThenElse

genAdd =
  Gen.subterm2 genNumeric genNumeric Add

genNumeric =
  Gen.recursive Gen.choice [genLitI] [genAdd, genITE genNumeric genNumeric]

genOr =
  Gen.subterm2 genBool genBool Or

