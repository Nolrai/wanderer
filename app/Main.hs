{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}

module Main (main) where

import Relude
import Wanderer
import Graphics.Gloss
import Linear

main :: IO ()
main = simulate displayMode black 32 start toPicture takeStep

displayMode :: Display
displayMode = InWindow ("Executable for " ++ projectName) (1000, 1000) (100, 100)

data SimState a = SimState {parity :: Bool, p :: V2 a, q :: V2 a}

start :: Num a => SimState a
start = SimState {parity = False, p = V2 300 0, q = V2 0 10}

onV2 :: (a -> a -> b) -> V2 a -> b
onV2 f (V2 x y) = f x y

toPicture :: SimState Float -> Picture
toPicture SimState {..} =
  pictures 
    [ color white $ circleSolid 10
    , onV2 translate p . color green $ circleSolid 5
    ]

-- Rotates clockwise
takeStepTest :: a -> Float -> SimState Float -> SimState Float
takeStepTest _ deltaT SimState {..} = SimState {parity = not parity, p = p', q = q'}
  where
    p' = p *! V2 (V2 (cos deltaT) (- (sin deltaT))) (V2 (sin deltaT) (cos deltaT))
    q' = q

takeStep :: a -> Float -> SimState Float -> SimState Float
takeStep _ deltaT SimState {..} = SimState {parity = not parity, p = p', q = q'}
  where
    (p', q') = euler (keplerG 1000000) hookF p q deltaT
    euler = if parity then semiImplicitEuler else semiImplicitEuler'