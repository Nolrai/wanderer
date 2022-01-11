{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings, ScopedTypeVariables #-}

module Main (main) where

import Relude
import Wanderer
import Graphics.Gloss
import Linear
import Turtle.Options as Opt

optParser :: Opt.Parser MyOptions
optParser = mkOptions 
  <$> argRead "law" "Which force law to use, Hook or Kepler"
  <*> optional (fromIntegral <$> optInt "forceConstant" 'k' "the scale constant for how strong the force is")
  <*> optional (uncurry V2 <$> optRead "startPosition" 'p' "") 
  <*> optional (uncurry V2 <$> optRead "startVelocity" 'q' "")
  <*> optional (fromIntegral <$> optInt "framesPerSecond" 'f' "")
  <*> optional (fromIntegral <$> optInt "stepsPerFrame" 's' "Steps of symplecticEuler to do per frame")

mkOptions :: Law
  -> Maybe Float
  -> Maybe (V2 Float)
  -> Maybe (V2 Float)
  -> Maybe Word8
  -> Maybe Word32
  -> MyOptions
mkOptions law k p' q' framesPerSecond' stepsPerFrame' = 
  let
  forceConstant = fromMaybe (if law == Hook then 0.1 else 10 ^ (5 :: Int)) k
  p = fromMaybe (V2 200 0) p'
  q = fromMaybe (V2 0 8) q'
  framesPerSecond = fromMaybe 16 framesPerSecond'
  stepsPerFrame = fromMaybe 1 stepsPerFrame'
  in MyOptions {..}

data MyOptions = MyOptions 
  { law :: Law
  , forceConstant :: Float
  , p :: V2 Float
  , q :: V2 Float
  , framesPerSecond :: Word8
  , stepsPerFrame :: Word32
  } 

main :: IO ()
main = do
  MyOptions {..} <- options (fromString projectName) optParser
  simulate 
    displayMode 
    black 
    (fromIntegral framesPerSecond) 
    (start p q) 
    toPicture 
    (takeStep law forceConstant stepsPerFrame)

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
