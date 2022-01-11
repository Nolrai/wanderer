{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Relude
import Wanderer
import Criterion
import Criterion.Main

instance NFData a => NFData (SimState a) where

deltaT :: Float
deltaT = 1/32

main :: IO ()
main = defaultMain 
  [ bgroup "takeStep'" [bench "takeStep'" $ nf (takeStep'   [] deltaT) start]
  , bgroup "takeStep" $ map benchItem (0 : [10 .. 18])
  ]

benchItem :: Int -> Benchmark
benchItem i = 
  bench ("takeStep " <> show n)
  $ nf (takeStep n [] deltaT) start
  where
    n = 2 ^ i