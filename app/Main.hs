module Main (main) where

import Wanderer (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
