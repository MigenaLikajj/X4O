module Common where

import System.Random

randomPick :: RandomGen g => [a] -> g -> (a, g)
randomPick xs gen = let (index, gen') = randomR (0, (length xs) - 1) gen
                    in (xs !! index, gen')
