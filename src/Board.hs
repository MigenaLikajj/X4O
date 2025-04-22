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

import qualified Data.Map.Strict as Map (fromList)
import Data.Map.Strict((!), Map)

-- Dimensionet e board
rows = 6
columns = 7

-- Aliaset e tipeve per qartesi
type Column = Int
type Row = Int
type Coords = (Column, Row)  -- Koordinata ne board

-- Reprezenton gjendjen e nje cell te vetme ose rezultatin e lojes
data Color = Empty | Red | Yellow | Both deriving (Eq, Show)

-- Reprezenton gjendjen e te gjithe board te lojes
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
