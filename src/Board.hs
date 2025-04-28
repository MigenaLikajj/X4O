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


-- Ekstrakton katershet vertikale
verticals :: Board -> [[Color]]
verticals b = [ [board b ! (x, y + i) | i <- [0..3]]
              | x <- [1..columns], y <- [1..(rows - 3)] ]

-- Ekstrakton katershet horizontale
horizontals :: Board -> [[Color]]
horizontals b = [ [board b ! (x + i, y) | i <- [0..3]]
                | x <- [1..(columns - 3)], y <- [1..rows] ]

-- Ekstrakton katershet diagonale te djathta ( \ forma )
rdiags :: Board -> [[Color]]
rdiags b = [ [board b ! (x + i, y + i) | i <- [0..3]]
           | x <- [1..(columns - 3)], y <- [1..(rows - 3)] ]

-- Ekstrakton katershet diagonale te majta ( \ forma )
ldiags :: Board -> [[Color]]
ldiags b = [ [board b ! (x + i, y - i) | i <- [0..3]]
           | x <- [1..(columns - 3)], y <- [4..rows] ]


-- Kontrolli master i fitores duke kombinuar te 4 drejtimet
checkWin :: Board -> Bool
checkWin board = or [f board | f <- [checkWinCol, checkWinRow, checkWinDiagRight, checkWinDiagLeft]]

-- Ndihmese e pergjithshme per te kontrolluar nese 4 copa me ngjyre te kundert jane ne nje rresht
checkWin' :: (Board -> [[Color]]) -> Board -> Bool
checkWin' f b = or $ map (and . map ((==) (opp $ color b))) $ f b

-- Kontrolle specifike te kushteve te fitores
checkWinCol :: Board -> Bool
checkWinCol = checkWin' verticals

checkWinRow :: Board -> Bool
checkWinRow = checkWin' horizontals

checkWinDiagRight :: Board -> Bool
checkWinDiagRight = checkWin' rdiags

checkWinDiagLeft :: Board -> Bool
checkWinDiagLeft = checkWin' ldiags

-- Calculate value of a particular board w.r.t to Color
valuation :: Board -> Int
valuation board = sum [f board | f <- [valuationCol, valuationRow, valuationDiagRight, valuationDiagLeft]]

-- Calculate score of each quad set and return the sum
valuation' :: (Board -> [[Color]]) -> Board -> Int
valuation' f b = sum $ map (scoreQuad $ color b) $ f b

valuationCol :: Board -> Int
valuationCol = valuation' verticals

valuationRow :: Board -> Int
valuationRow = valuation' horizontals

valuationDiagRight :: Board -> Int
valuationDiagRight = valuation' rdiags

valuationDiagLeft :: Board -> Int
valuationDiagLeft = valuation' ldiags

scoreQuad :: Color -> [Color] -> Int
scoreQuad color colors = if (and [ c == Empty || c == color | c <- colors])
                         then case (length $ filter (== color) colors) of 3 -> 1
                                                                          _ -> 0
                         else 0

instance GamePosition Board where
  moves :: Board -> [Board]
  moves b = map (makeMove b) $ possibleMoves b

  static :: Board -> Int
  static b = if (checkWin b) then -100 else (valuation b) - (valuation b { color = opp (color b) })

instance Show Board where
  show :: Board -> String
  show board' = let b = board board'
                    strBoard = [[if z == Red then "R" else if z == Yellow then "Y" else " " | x <- [1..columns], let z = b ! (x, y)] | y <- [rows,(rows-1)..1]]
                    rowSep = "\n" ++ (concat $ replicate columns "+---") ++ "+\n"
                    output = intercalate rowSep $ map (\x ->"| " ++ (intercalate " | " x) ++ " |") strBoard
                in rowSep ++ output ++ rowSep
