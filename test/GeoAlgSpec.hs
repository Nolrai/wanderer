{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GeoAlgSpec where

import GeoAlg
import Relude
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.SimpleReflect ( var, Expr )

data GnomeTest = GnomeTest ![Int] !Int
  deriving stock (Show)

instance Arbitrary GnomeTest where
  arbitrary = do
    l <- arbitrary
    n <- choose (0, length l)
    pure $ GnomeTest l n

swapAt :: Int -> [a] -> [a]
swapAt n l =
  case (reverse left, right) of
  (x:xs, y:ys) -> reverse xs ++ y : x : ys
  (_xs, _ys) -> l
  where
  (left, right) = splitAt (n + 1) l

spec :: Spec
spec = do
  describe "swapAt" $ do
    prop "just shuffles" $
      \ (GnomeTest l n) -> sort (swapAt n l) `shouldBe` sort l
  describe "gnomeSort" $ do
    prop "leaves unsorted alone" $
      \ (Sorted l :: SortedList Int) ->
        insertSort l `shouldBe` (Pos, l)
    prop "makes unsorted lists sorted" $
      \ (l :: [Int]) -> within 100000 (snd (insertSort l) `shouldBe` sort l)
    prop "tracks permutation sign" $
      \ (GnomeTest l i) -> do
        let a = insertSort l
        let b = insertSort (swapAt i l)
        (i + 1 < length l && (l /= swapAt i l)) ==>
          a `shouldBe` first negate b

  describe "MultiVector" $ do
    prop "has a Arbitrary instance" $
      \ (x :: MultiVector Axis Float) -> x `shouldBe` x
    describe "is a ring (when its scalars are)" $ do
      prop "has an additive idenity" $
        \ (x :: MultiVector Axis Int) -> x + 0 `shouldBe` x
      prop "has a multiplicative idenity" $
        \ (x :: MultiVector Axis Int) -> x * 1 `shouldBe` x
      prop "has communitive addition" $
        \ (x :: MultiVector Axis Int) y -> x + y `shouldBe` y + x
      prop "has absorbsive 0" $
        \ (x :: MultiVector Axis Int) -> x * 0 `shouldBe` 0
      prop "multiplication distributes over addition" $
        \ (x :: MultiVector Axis Int) (y :: MultiVector Axis Int) (z :: MultiVector Axis Int) ->
          x * (y + z) `shouldBe` (x * y) + (x * z)
      prop "self multiplying a k-vector produces a scalar" $
        \ (HMV x :: HomogeniousMV Axis Int) ->
          x * x `shouldSatisfy` isScalar
      -- it "multiplies" $ do
      --   printLines `mapM_` toLines (multiVectorExample "a" * multiVectorExample "b")
      --   True `shouldBe` False
    prop "folows QuadForm" $ do
      \ (t :: Axis) -> (baseV t * baseV t :: MultiVector Axis Int) `shouldBe` fromInteger (quadForm t)
    describe "contains the Integers" $ do
      prop "fromInteger is additive homomorphism" $
        \ (x :: Integer) (y :: Integer) -> (fromInteger x :: MultiVector Axis Int) + fromInteger y `shouldBe` fromInteger (x + y :: Integer)
      prop "fromInteger is multiplicative homomorphism" $
        \ (x :: Integer) (y :: Integer) -> (fromInteger x :: MultiVector Axis Int) * fromInteger y `shouldBe` fromInteger (x * y :: Integer)

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

newtype HomogeniousMV basis a = HMV (MultiVector basis a)
  deriving newtype (Show)

instance (Ord basis, Arbitrary basis, Arbitrary t) => Arbitrary (HomogeniousMV basis t) where
  arbitrary = HMV <$> arbitrary `suchThat` isHomogenious
  shrink (HMV m) = HMV <$> filter isHomogenious (shrink m)

isHomogenious :: MultiVector basis a -> Bool
isHomogenious (MkMultiVector m) = Set.size (Map.keysSet m) <= 1

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