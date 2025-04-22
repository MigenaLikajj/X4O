module AI where

import System.Random

import Common (randomPick)

class GamePosition a where
  moves :: a -> [a]
  static :: a -> Int
