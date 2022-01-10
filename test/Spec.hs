module Main (main) where

import Wanderer (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
