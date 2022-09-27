{-# LANGUAGE NoImplicitPrelude, DataKinds, TypeFamilies, DerivingStrategies, ScopedTypeVariables #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module GeoAlg where
import Relude
import Linear ( (!*!), scaled, M44, V4(V4) )
import Data.Complex as Complex ( conjugate, realPart, Complex(..) )
import Data.Vector as Vector ( fromList, sum, zipWith, Vector )
import Data.Map.Strict as Map ( fromList, toAscList, mapKeysMonotonic, foldrWithKey, unionWith, intersectionWith, lookup, filter )
import Data.Set as Set ( foldl', empty )

data Axis = T | X | Y | Z
  deriving stock (Show, Read, Ord, Eq, Enum, Bounded)

newtype STA a = MkSTA {asMatrix :: M44 (Complex a)}

asVector :: STA a -> Vector (Complex a)
asVector MkSTA {asMatrix = m} =
  case m of
  V4
    (V4  a00 a01 a02 a03)
    (V4  a10 a11 a12 a13)
    (V4  a20 a21 a22 a23)
    (V4  a30 a31 a32 a33) ->
      Vector.fromList
        [ a00, a01, a02, a03
        , a10, a11, a12, a13
        , a20, a21, a22, a23
        , a30, a31, a32, a33
        ]

diracGamma :: RealFloat a => Axis -> M44 (Complex a)
diracGamma T = V4
  (V4  1  0   0    0  )
  (V4  0  1   0    0  )
  (V4  0  0 (-1)   0  )
  (V4  0  0   0  (-1) )

diracGamma X = V4
  (V4   0    0   0  1 )
  (V4   0    0   1  0 )
  (V4   0  (-1)  0  0 )
  (V4 (-1)   0   0  0 )

diracGamma Y = V4
  (V4   0  0  0  (-j) )
  (V4   0  0  j    0 )
  (V4   0  j  0    0 )
  (V4 (-j) 0  0    0 )

diracGamma Z = V4
  (V4   0  0  1   0  )
  (V4   0  0  0 (-1) )
  (V4 (-1) 0  0   0  )
  (V4   0  1  0   0  )

j :: Num a => Complex a
j = 0 :+ 1

newtype FreeModule basis a = MkFreeModule [([basis], a)]

newtype MultiVector basis a = MkMultiVector (Map (Set basis) a)
  deriving newtype (Eq, Show, Read)

reduce :: forall basis a. (Ord basis, Num a) => (basis -> a) -> [basis] -> (a, Set basis)
reduce f l = (sig, Relude.fromList sorted)
  where
    (sig, sorted) = go l
    go :: [basis] -> (a, [basis])
    go (x:y:ys)
      | x < y = let (a', l') = go (y : ys) in (a', x : l')
      | x == y = let (a', l') = go ys in (f x * a', l')
      | otherwise =  let (a', l') = go (y:x:ys) in (-1 * a', l')
    go l' = (1, l')

fmProd :: Num a => FreeModule basis a -> FreeModule basis a -> FreeModule basis a
fmProd (MkFreeModule left) (MkFreeModule right) =
  MkFreeModule $ do
    (leftKey, leftValue) <- left
    (rightKey, rightValue) <- right
    return (leftKey <> rightKey, leftValue * rightValue)

toMV :: (Ord basis, Num a) => (basis -> a) -> FreeModule basis a -> MultiVector basis a
toMV f (MkFreeModule l) = MkMultiVector . Map.fromList . fmap (\ (k, a) -> let (sig, k') = reduce f k in (k', sig * a)) $ l

fromMV :: MultiVector basis a -> FreeModule basis a
fromMV (MkMultiVector m) = MkFreeModule . Map.toAscList . Map.mapKeysMonotonic Relude.toList $ m

class Ord basis => QuadForm basis where
  -- what does x * x reduce to?
  quadForm :: Num a => basis -> a

instance QuadForm Axis where
  quadForm :: Num a => Axis -> a
  quadForm T = 1
  quadForm _ = -1

mvProd :: (QuadForm basis, Num a) => MultiVector basis a -> MultiVector basis a -> MultiVector basis a
mvProd left right =
  toMV quadForm $ fmProd (fromMV left) (fromMV right)

toSTA :: RealFloat a => MultiVector Axis a -> STA a
toSTA (MkMultiVector m) = MkSTA $
  Map.foldrWithKey (\ k a b -> fromBlade k a + b) 0 m

fromBlade :: forall a. RealFloat a => Set Axis -> a -> M44 (Complex a)
fromBlade set size = Set.foldl' (\ a b -> a !*! diracGamma b) diag set
  where
  diag :: M44 (Complex a)
  diag = Linear.scaled (V4 size' size' size' size')
  size' :: Complex a
  size' = size :+ 0

-- complex Frobenius inner product
cdot :: RealFloat a => STA a -> STA a -> a
cdot a b = realPart $ asVector a `cdot'` asVector b

cdot' :: RealFloat a => Vector (Complex a) -> Vector (Complex a) -> Complex a
cdot' x y = Vector.sum $ Vector.zipWith (\ a b -> a * Complex.conjugate b) x y

instance (Eq a, Fractional a, QuadForm basis) =>
  Num (MultiVector basis a) where
    (+) :: MultiVector basis a -> MultiVector basis a -> MultiVector basis a
    MkMultiVector a + MkMultiVector b = MkMultiVector . Map.filter (/= 0) $ Map.unionWith (+) a b
    (*) :: MultiVector basis a -> MultiVector basis a -> MultiVector basis a
    (*) = mvProd
    negate :: MultiVector basis a -> MultiVector basis a
    negate (MkMultiVector m) = MkMultiVector (negate <$> m)
    fromInteger :: Integer -> MultiVector basis a
    fromInteger n = fromX fromInteger n

fromX :: (Eq a, Num a, Ord basis) => (t -> a) -> t -> MultiVector basis a
fromX f x = MkMultiVector . Map.filter (/= 0) $ Map.fromList [(Set.empty, f x)]

rank0 :: (Num a, Ord basis) => MultiVector basis a -> a
rank0 (MkMultiVector m) =
  fromMaybe 0 $ Map.lookup Set.empty m

