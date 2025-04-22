module AI where

import System.Random

import Common (randomPick)

class GamePosition a where
  moves :: a -> [a]
  static :: a -> Int

-- We take maximum of the negative of the scores
gatherMax :: [(Int, ((Maybe Int, Int), a))] -> (Int, ((Maybe Int, Int), a)) -> [(Int, ((Maybe Int, Int), a))]
gatherMax [] x = x:[]
gatherMax xs@((_, ((_, bscore), _)):_) x@(_, ((_, score), _)) =
  if score == bscore
  then x:xs
  else if (score < bscore)
       then x:[]
       else xs

genRandom :: (RandomGen g) => g -> Int -> [StdGen] -> ([StdGen], g)
genRandom orig 0 gs = (gs, orig)
genRandom orig x gs = let (seed, orig') = random orig
                          g = mkStdGen seed
                      in genRandom orig' (x - 1) (g:gs)

negmax :: (GamePosition a, RandomGen g) => Int -> a -> g -> ((Maybe Int, Int), g)
negmax depth b gen
 | depth == 0     = ((Nothing, static b), gen)
 | null (moves b) = ((Nothing, static b), gen)
 | otherwise =
     let posMoves      = moves b
         (gens, gen')  = genRandom gen (length posMoves) []
         results       = zipWith (\g mov -> negmax (depth - 1) mov g) gens (moves b)
         zipped        = zip [0..] results
         bests         = foldl gatherMax [] zipped
         (best, gen'') = randomPick bests gen'
         (moveIndex, ((_, score), _)) = best
     in ((Just moveIndex, negate score), gen'')                     