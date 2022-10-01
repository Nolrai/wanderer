{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# LANGUAGE InstanceSigs #-}

module GeoAlgSpec where

import GeoAlg
import Relude
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.SimpleReflect ( var, Expr )

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
      prop "multiplication distributes over addition" $
        \ (x :: MultiVector Axis Rational) (y :: MultiVector Axis Rational) (z :: MultiVector Axis Rational) -> 
          x * (y + z) `shouldBe` (x * y) + (x * z)
      prop "selfMultiplication produces a scalar" $
        \ (x :: MultiVector Axis Rational) ->
          isScalar (x * x)
      it "multiplies" $ do
        printLines `mapM_` toLines (multiVectorExample "a" * multiVectorExample "b")
        True `shouldBe` False
    prop "folows QuadForm" $ do
      \ (t :: Axis) -> (baseV t * baseV t :: MultiVector Axis Rational) `shouldBe` fromInteger (quadForm t)
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
  shrink :: MultiVector basis t -> [MultiVector basis t]
  shrink (MkMultiVector m) = fmap MkMultiVector (shrink m)

multiVectorExample :: [Char] -> MultiVector Axis' Expr
multiVectorExample c = MkMultiVector . Map.fromList $ do
  (basis :: [Axis']) <- subsequences [minBound .. maxBound]
  return (Set.fromAscList basis, toVarName c basis)

toVarName :: (Eq b, Bounded b, Enum b) => String -> [b] -> Expr
toVarName c basis = var (c <> toSubscript basis)
  where
  toSubscript b = (\ t -> if t `elem` b then '1' else '0') `map` [minBound .. maxBound]

printLines :: (Set Axis', Expr) -> IO ()
printLines (k, rhs) = 
  putTextLn $ show lhs <> fromString " = " <> show rhs
  where
    lhs :: Expr
    lhs = toVarName "c". Set.toAscList $ k

toLines :: MultiVector basis a -> [(Set basis, a)]
toLines (MkMultiVector m) = Map.toAscList m