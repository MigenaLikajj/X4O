module AI where

import System.Random

import Common (randomPick)

class GamePosition a where
  moves :: a -> [a]
  static :: a -> Int

-- We take maximum of the negative of the scores
-- Mban vetem levizjet qe kane piket me te larta nga te gjitha levizjet e analizuara.
-- Nese ka disa me pike te barabarta i mban te gjitha ato
gatherMax :: [(Int, ((Maybe Int, Int), a))] -> (Int, ((Maybe Int, Int), a)) -> [(Int, ((Maybe Int, Int), a))]
gatherMax [] x = x:[]
gatherMax xs@((_, ((_, bscore), _)):_) x@(_, ((_, score), _)) =
  if score == bscore
  then x:xs
  else if (score < bscore)
       then x:[]
       else xs

-- Perdoret per gjenerimin e numrave random per vendosjen e diskut ne kolonat qe ka hapsire
genRandom :: (RandomGen g) => g -> Int -> [StdGen] -> ([StdGen], g)
genRandom orig 0 gs = (gs, orig)
genRandom orig x gs = let (seed, orig') = random orig
                          g = mkStdGen seed
                      in genRandom orig' (x - 1) (g:gs)
-- Algoritmi kryesore qe analizon te gjitha levizjet e mundeshme deri ne nje thellesi te caktuar dhe zgjedh
-- njeren prej me te mirave bazuar  ne piket e vleresimit
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