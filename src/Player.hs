module Player where

import System.Random

import AI (GamePosition, negmax)
import Common (randomPick)

type Player a b = [a] -> b -> Maybe a -> IO a

humanPlayer :: GamePosition b => Player a b
humanPlayer moves b (Just c) = return c
humanPlayer moves b Nothing = error "i need an IO entry to work"

randplayer :: Player a b
randplayer moves _ _ = getStdRandom (randomPick moves)

negmaxplayer :: GamePosition b => Int -> Player a b
negmaxplayer depth moves b _ =
  do
    (Just index, _) <- getStdRandom (negmax depth b)
    return $ moves !! index
