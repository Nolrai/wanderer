{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}

module GeoAlgSpec (spec) where

import GeoAlg
import Relude
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "MultiVector" $ do
    prop "has a Arbitrary instance" $
      \ (x :: MultiVector Axis Float) -> x `shouldBe` x
    describe "is a ring (when its scalars are): " $ do
      prop "has an additive idenity" $
        \ (x :: MultiVector Axis Rational) -> x + 0 `shouldBe` x
      prop "has a multiplicative idenity" $
        \ (x :: MultiVector Axis Rational) -> x * 1 `shouldBe` x
      prop "has communitive addition" $
        \ (x :: MultiVector Axis Rational) y -> x + y `shouldBe` y + x
      prop "has absorbsive 0" $
        \ (x :: MultiVector Axis Rational) -> x * 0 `shouldBe` 0
    describe "contains the Integers" $ do
      prop "fromInteger is additive homomorphism" $
        \ (x :: Integer) (y :: Integer) -> (fromInteger x :: MultiVector Axis Rational) + fromInteger y `shouldBe` fromInteger (x + y :: Integer)
      prop "fromInteger is multiplicative homomorphism" $
        \ (x :: Integer) (y :: Integer) -> (fromInteger x :: MultiVector Axis Rational) * fromInteger y `shouldBe` fromInteger (x * y :: Integer)

instance Arbitrary Axis where
  arbitrary = sized $ chooseEnum . arbitraryRange
  shrink x = filter (< x) [minBound .. maxBound]

arbitraryRange :: Int -> (Axis, Axis)
arbitraryRange size
  | size < 3 = (T, T)
  | size < 5 = (T, X)
  | otherwise = (T, Z)

instance (Ord basis, Arbitrary basis, Arbitrary t) => Arbitrary (MultiVector basis t) where
  arbitrary = do
    dims <- listOf orderedList
    values <- vector (length dims)
    pure . MkMultiVector . Map.fromList $ zipWith (\ k a -> (Set.fromList k, a)) dims values
