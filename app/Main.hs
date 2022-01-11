{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}

module Main (main) where

import Relude
import Wanderer
import Graphics.Gloss
import Linear

main :: IO ()
main = simulate displayMode black 16 start toPicture (takeStep 1)

numSubSteps :: Int
numSubSteps = 2 ^ e
  where
    e :: Int
    e = 0

displayMode :: Display
displayMode = InWindow ("Executable for " ++ projectName) (1000, 1000) (100, 100)

onV2 :: (a -> a -> b) -> V2 a -> b
onV2 f (V2 x y) = f x y

toPicture :: SimState Float -> Picture
toPicture SimState {..} =
  pictures 
    [ color white $ circleSolid 10
    , onV2 translate p . color green $ circleSolid 5
    , color (greyN 0.8) $ line (map (\ (V2 x y) -> (x,y)) path)
    ]
