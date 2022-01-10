module Main (main) where

import Wanderer (projectName)


main :: IO ()
main = putStrLn ("Benchmarks for " ++ projectName)
