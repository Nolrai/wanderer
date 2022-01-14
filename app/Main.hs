{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings, ScopedTypeVariables, DerivingStrategies #-}

module Main (main) where

import Relude
import Wanderer
import Graphics.Gloss
import Linear
import Turtle.Options as Opt

optParser :: Opt.Parser MyOptions
optParser = mkOptions 
  <$> (realToFrac <$> argDouble "law" "What power of the distance to divide by: Hook is 1, Kepler is 2")
  <*> switch "increasing" 'i' "Does the force increase from the center like Hook's Law"
  <*> optional (fromIntegral <$> optInt "forceConstant" 'k' "the scale constant for how strong the force is")
  <*> optional (uncurry V2 <$> optRead "startPosition" 'p' "") 
  <*> optional (uncurry V2 <$> optRead "startVelocity" 'q' "")
  <*> optional (fromIntegral <$> optInt "framesPerSecond" 'f' "")
  <*> optional (fromIntegral <$> optInt "stepsPerFrame" 's' "Steps of symplecticEuler to do per frame")
  <*> optional (fromIntegral <$> optInt "secondsPerSecond" 't' "a time scale factor")

mkOptions :: Float
  -> Bool
  -> Maybe Float
  -> Maybe (V2 Float)
  -> Maybe (V2 Float)
  -> Maybe Word8
  -> Maybe Word32
  -> Maybe Float
  -> MyOptions
mkOptions power' sign k p' q' framesPerSecond' stepsPerFrame' secondsPerSecond' = 
  let
  power = if sign then -power' else power'
  forceConstant = fromMaybe (norm q / ( 2 * norm (accFromP power 1 p))) k
  p = fromMaybe (V2 300 0) p'
  q = fromMaybe (V2 0 100) q'
  framesPerSecond = fromMaybe 16 framesPerSecond'
  stepsPerFrame = fromMaybe 20 stepsPerFrame'
  secondsPerSecond = fromMaybe 1 secondsPerSecond'
  in MyOptions {..}

data MyOptions = MyOptions 
  { power :: Float
  , forceConstant :: Float
  , p :: V2 Float
  , q :: V2 Float
  , framesPerSecond :: Word8
  , stepsPerFrame :: Word32
  , secondsPerSecond :: Float
  } deriving stock (Show)

main :: IO ()
main = do
  s@MyOptions {..} <- options (fromString projectName) optParser
  print s
  simulate 
    displayMode 
    black 
    (fromIntegral framesPerSecond) 
    (start p q) 
    toPicture 
    (\ viewPort deltaT -> takeStep power forceConstant stepsPerFrame viewPort (deltaT * secondsPerSecond))

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
