{-# LANGUAGE NoImplicitPrelude, RecordWildCards, DeriveGeneric, DerivingStrategies #-}

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
    , keplerF
    , keplerG
    , hookF
    , hookG
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
    x' = x + f v
    v' = v + g x' ^* delta_t

-- I'm not sure if any phisics sim has something other than this..
keplerF, hookF :: V2 Float -> V2 Float 
keplerF v = v
hookF = keplerF

keplerG, hookG :: Float -> V2 Float -> V2 Float 
keplerG scale x = - scale *^ signorm x ^/ quadrance x
hookG omega x = (- omega * omega) *^ x

data SimState a = SimState {parity :: Bool, p :: V2 a, q :: V2 a, path :: [V2 a]}
  deriving stock (Show, Read, Eq, Ord, Generic)

takeStep :: Int -> a -> Float -> SimState Float -> SimState Float
takeStep numSubSteps a deltaT = 
  traceShow ("takeStep", numSubSteps, "", deltaT)
  . (!! numSubSteps) 
  . S.iterate (takeStep' a (deltaT / fromIntegral numSubSteps)) 

takeStep' :: a -> Float -> SimState Float -> SimState Float
takeStep' _ deltaT SimState {..} = SimState {parity = not parity, p = p', q = q', path = p : path}
  where
    (p', q') = euler (keplerG 200000) hookF p q deltaT
    euler = if parity then semiImplicitEuler else semiImplicitEuler'

start :: Num a => SimState a
start = SimState {parity = False, p = V2 200 0, q = V2 0 8, path = []}