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
    ) where

import Relude
import Linear

projectName :: String
projectName = "wanderer"

semiImplicitEuler, semiImplicitEuler' :: 
  (t -> V2 Double -> V2 Double)
  -> (t -> V2 Double -> V2 Double)
  -> V2 Double
  -> V2 Double
  -> t
  -> Double
  -> (V2 Double, V2 Double)
semiImplicitEuler g f x v t delta_t = (x', v')
  where
    v', x' :: V2 Double
    v' = v + g t x ^* delta_t
    x' = x + f t v' ^* delta_t

semiImplicitEuler' g f x v t delta_t = (x', v')
  where
    x', v' :: V2 Double
    x' = x + f t v
    v' = v + g t x' ^* delta_t

keplerF, keplerG :: Double -> V2 Double -> V2 Double 
keplerF _ v = v
keplerG _ x = signorm x ^/ quadrance x