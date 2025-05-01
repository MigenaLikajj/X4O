module UIBoard (drawBoard) where

import Data.Map.Strict((!))

import Graphics.Gloss

import qualified Board

gameCoin :: Picture
gameCoin = circleSolid 40

convColor :: Board.Color -> (Picture -> Picture)
convColor Board.Empty = color white
convColor Board.Red = color red
convColor Board.Yellow = color yellow

drawBoard' :: Board.Board -> Picture
drawBoard' b = pictures [translate' x y $ convColor (board ! (x, y)) $ gameCoin | x <- [1..Board.columns], y <- [1..Board.rows]]
  where
    board = Board.board b