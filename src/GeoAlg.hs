{-# LANGUAGE NoImplicitPrelude, DataKinds, TypeFamilies, DerivingStrategies, ScopedTypeVariables #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module GeoAlg where
import Relude
import Linear ( (!*!), scaled, M44, V4(V4) )
import Data.Complex as Complex ( conjugate, realPart, Complex(..) )
import Data.Vector as Vector ( fromList, sum, zipWith, Vector )
import Data.Map.Strict as Map ( fromList, toAscList, mapKeysMonotonic, foldrWithKey, unionWith, lookup, filter, singleton, insertWithKey, mapWithKey, mapKeysWith, fromListWith, keysSet, keys )
import Data.Set as Set ( foldl', empty, singleton, size, fromList, isSubsetOf, insert )
import Text.Show (Show(show))

data Axis = T | X | Y | Z
  deriving stock (Show, Read, Ord, Eq, Enum, Bounded)

data Axis' = X' | Y'
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

data Sig = Neg | Zero | Pos
  deriving stock (Eq, Enum)

instance Show Sig where
  show :: Sig -> String
  show x = Text.Show.show (withSig x 1 :: Int)

instance Num Sig where
  (*) = withSig
  fromInteger 0 = Zero
  fromInteger n
    | n > 0 = Pos
    | otherwise = Neg
  negate Pos = Neg
  negate Zero = Zero
  negate Neg = Pos
  Zero + x = x
  x + Zero = x
  x + y 
    | x == y = x
    | otherwise = Zero
  abs = const Pos
  signum = id

withSig :: Num a => Sig -> a -> a
withSig Zero = const 0
withSig Pos = id
withSig Neg = negate

reduce :: forall basis. (Ord basis) => (basis -> Sig) -> [basis] -> (Sig, Set basis)
reduce f l = (sig `withSig` sig', contracted)
  where
    (sig', contracted) = contract f sorted
    (sig, sorted) = gnome Pos [] l

gnome :: Ord a => Sig -> [a] -> [a] -> (Sig, [a])
gnome sig l [] = (sig, l)
gnome sig [] (y : ys) = gnome sig [y] ys
gnome sig (x:xs) (y:ys)
  | x < y = gnome (negate sig) (y:xs) (x:ys)
  | otherwise = gnome sig xs (y : x : ys)

contract :: (Ord a) => (a -> Sig) -> [a] -> (Sig, Set a)
contract _ [] = (1, Set.empty)
contract _ [x] = (1, Set.singleton x)
contract f (x : y : ys)
  | x == y = let (sig, set) = contract f ys in (f x * sig, set)
  | otherwise = let (sig, set) = contract f (y : ys) in (sig, Set.insert x set)

fmProd :: Num a => FreeModule basis a -> FreeModule basis a -> FreeModule basis a
fmProd (MkFreeModule left) (MkFreeModule right) =
  MkFreeModule $ do
    (leftKey, leftValue) <- left
    (rightKey, rightValue) <- right
    return (leftKey <> rightKey, leftValue * rightValue)

toMV :: (Ord basis, Num a) => (basis -> Sig) -> FreeModule basis a -> MultiVector basis a
toMV f (MkFreeModule l) = MkMultiVector .
  Map.fromListWith (+) .
  map (\ (k, a) -> let (sig, k') = reduce f k in (k', sig `withSig` a)) $
  l

fromMV :: MultiVector basis a -> FreeModule basis a
fromMV (MkMultiVector m) = MkFreeModule . Map.toAscList . Map.mapKeysMonotonic Relude.toList $ m

class Ord basis => QuadForm basis where
  -- what does x * x reduce to?
  quadForm :: Num a => basis -> a

instance QuadForm Axis where
  quadForm :: Num a => Axis -> a
  quadForm T = -1
  quadForm _ = 1

instance QuadForm Axis' where
  quadForm :: Num a => Axis' -> a
  quadForm _ = 1

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

removeZeros :: (Eq a, Num a) => MultiVector basis a -> MultiVector basis a
removeZeros (MkMultiVector m) = MkMultiVector . Map.filter (/= 0) $ m

instance (Eq a, Fractional a, QuadForm basis) =>
  Num (MultiVector basis a) where
    (+) :: MultiVector basis a -> MultiVector basis a -> MultiVector basis a
    MkMultiVector a + MkMultiVector b = removeZeros . MkMultiVector $ Map.unionWith (+) a b
    (*) :: MultiVector basis a -> MultiVector basis a -> MultiVector basis a
    a * b = removeZeros $ mvProd a b
    negate :: MultiVector basis a -> MultiVector basis a
    negate (MkMultiVector m) = MkMultiVector (negate <$> m)
    fromInteger :: Integer -> MultiVector basis a
    fromInteger n = fromX fromInteger n

fromX :: (Eq a, Num a, Ord basis) => (t -> a) -> t -> MultiVector basis a
fromX f x = MkMultiVector . Map.filter (/= 0) $ Map.fromList [(Set.empty, f x)]

rank0 :: (Num a, Ord basis) => MultiVector basis a -> a
rank0 (MkMultiVector m) =
  fromMaybe 0 $ Map.lookup Set.empty m

baseV :: (Ord basis, Num a) => basis -> MultiVector basis a
baseV k = MkMultiVector $ Map.fromList [(Set.singleton k, 1)]

base :: (QuadForm basis, Num a) => [basis] -> MultiVector basis a
base l = toMV quadForm $ MkFreeModule [(l, 1)]

getRanks :: MultiVector a1 a2 -> Set Int
getRanks (MkMultiVector m) = Set.fromList . fmap Set.size . Map.keys $ m

isScalar :: MultiVector a1 a2 -> Bool
isScalar m = getRanks m `Set.isSubsetOf` Set.singleton 0