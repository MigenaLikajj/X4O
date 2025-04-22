-- Board.hs
-- Definon strukturen dhe gjendjen fillestare te lojes Connect Four

module Board (
  board,
  color,
  winner,
  rows,
  columns,
  Color (Empty, Red, Yellow, Both),
  Board (Board),
  Column,
  initialBoard
) where

import qualified Data.Map.Strict as Map (insert, fromList)
import Data.Map.Strict((!), Map)

-- Dimensionet e board
rows = 6
columns = 7

-- Aliaset e tipeve per qartesi
type Column = Int
type Row = Int
type Coords = (Column, Row)  -- Koordinata ne board

-- Numerimi per 3 ngjyrat dhe 4 mundesite e fitores
data Color = Empty | Red | Yellow | Both deriving (Eq, Show)

-- Tabela eshte vetem nje liste 2-dimensionale me lartesi
data Board = Board {
  board :: Map Coords Color,     
  heights :: Map Column Row,
  color :: Color,               
  winner :: Color                
}

-- Krijon nje tabele te re bosh me te gjitha qelizat bosh dhe me lojtarin e kuq si fillestar.
initialBoard = Board {
  board = Map.fromList [ ((x, y), Empty) | x <- [1..columns], y <- [1..rows] ],
  heights = Map.fromList [ (col, 0) | col <- [1..columns] ],
  color = Red,
  winner = Empty
}

-- Kthen kolonat, rreshti me i larte i te cilave nuk eshte ende i mbushur
possibleMoves :: Board -> [Column]
possibleMoves b
  | winner b /= Empty = []
  | otherwise = [x | x <- [1..columns], (heights b ! x) /= rows]
  where
    heights' = heights b

-- Perditeso tabelen
makeMove :: Board -> Column -> Board
makeMove b@Board{ heights = heights', color = c, board = board' } col =
  let posMoves = possibleMoves b
      curHeight = heights' ! col
      nBoard = Map.insert (col, curHeight + 1) c board'
      nHeights = Map.insert col (curHeight + 1) heights'
  in if col `elem` posMoves
     then b { board = nBoard, heights = nHeights, color = opp c }
     else b


opp :: Color -> Color
opp Red = Yellow
opp Yellow = Red
opp x = x