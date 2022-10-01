{-# LANGUAGE NoImplicitPrelude, RecordWildCards, NamedFieldPuns, DeriveGeneric, DerivingStrategies #-}

{- |
Copyright: (c) 2022 Chris Upshaw (From Yukari)
SPDX-License-Identifier: MIT
Maintainer: Chris Upshaw (From Yukari) <chrisaupshaw@gmail.com>

See README for more info
-}

module Wanderer
    ( projectName
    , semiImplicitEuler
    , semiImplicitEuler'
    , accFromP
    , velFromQ
    , start
    , SimState (..)
    , takeStep
    , takeStep'
    ) where

import Relude
import Linear
import Data.Stream.Infinite as S

projectName :: String
projectName = "wanderer"

-- https://en.wikipedia.org/wiki/Semi-implicit_Euler_method
-- The F/G notation is from the wikipedia page.. might be bad for physics sims.

semiImplicitEuler, semiImplicitEuler' :: 
  (V2 Float -> V2 Float)
  -> (V2 Float -> V2 Float)
  -> V2 Float
  -> V2 Float
  -> Float
  -> (V2 Float, V2 Float)
semiImplicitEuler g f x v delta_t = (x', v')
  where
    v', x' :: V2 Float
    v' = v + g x ^* delta_t
    x' = x + f v' ^* delta_t

semiImplicitEuler' g f x v delta_t = (x', v')
  where
    x', v' :: V2 Float
    x' = x + f v ^* delta_t
    v' = v + g x' ^* delta_t

-- Compute the acceleration from the position value
accFromP :: Float -> Float -> V2 Float -> V2 Float
accFromP power scale x = (- scale / (quadrance x ** (power / 2))) *^ signorm x

-- Compute the velocity from the momentum
velFromQ :: V2 Float -> V2 Float
velFromQ = id

data SimState a = SimState {parity :: !Bool, p :: !(V2 a), q :: !(V2 a), path :: ![V2 a]}
  deriving stock (Show, Read, Eq, Ord, Generic)

embed :: V2 Float -> V3 Float
embed (V2 x y) = V3 x y 0

takeStep :: Float -> Float -> Word32 -> t -> Float -> SimState Float -> SimState Float
takeStep power scale numSubSteps _ deltaT s =
  traceShow (embed (p s) `cross` embed (q s))
  shrinkPath
  . (!! fromIntegral numSubSteps) 
  . S.iterate (takeStep' power scale (deltaT / fromIntegral numSubSteps))
  $ s

shrinkPath :: SimState a -> SimState a
shrinkPath SimState {..} = (SimState {..}) {path = path'}
  where
    path' = if length path > 10 ^ (5 :: Word8) then seive path else path

seive :: [a] -> [a]
seive (x:_:xss) = x : seive xss
seive xss = xss

takeStep' :: Float -> Float -> Float -> SimState Float -> SimState Float
takeStep' power scale deltaT SimState {..} = SimState {parity = not parity, p = p', q = q', path = recordP p path}
  where
    (p', q') = euler (accFromP power scale) velFromQ p q deltaT
    euler = if parity then semiImplicitEuler else semiImplicitEuler'

-- only add p to the path if it's more than half a pixle away from the tip.
recordP :: (Ord a, Metric f, Floating a) => f a -> [f a] -> [f a]
recordP p [] = [p]
recordP p old@(x:_) = if distance p x > 0.5 then p:old else old

start :: V2 a -> V2 a -> SimState a
start p q = SimState {parity = False, p, q, path = []}