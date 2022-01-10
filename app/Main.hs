{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Relude
import Wanderer (projectName)
import Graphics.Gloss

main :: IO ()
main = display (InWindow ("Executable for " ++ projectName) (200, 200) (10, 10)) white (Circle 80)
