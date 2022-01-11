{-# LANGUAGE NoImplicitPrelude #-}

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
    ) where

import Relude
import Linear

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