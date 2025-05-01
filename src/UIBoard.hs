module UIBoard (drawBoard) where

import Data.Map.Strict((!))

import Graphics.Gloss

import qualified Board

gameCoin :: Picture
gameCoin = circleSolid 40

transRed :: Color
transRed = makeColorI 255 0 0 192

transYellow :: Color
transYellow = makeColorI 255 255 0 192

convColor :: Board.Color -> (Picture -> Picture)
convColor Board.Empty = color white
convColor Board.Red = color red
convColor Board.Yellow = color yellow

manipX :: Int -> Float
manipX x = fromIntegral (x - 4) * 100

manipY :: Int -> Float
manipY y = fromIntegral (2 * y - 7) * 50

translate' :: Int -> Int -> (Picture -> Picture)
translate' x y = translate (manipX x) (manipY y)

drawBoard' :: Board.Board -> Picture
drawBoard' b = pictures [translate' x y $ convColor (board ! (x, y)) $ gameCoin | x <- [1..Board.columns], y <- [1..Board.rows]]
  where
    board = Board.board b

drawBoard :: Board.Board -> IO Picture
drawBoard b@(Board.Board _ _ _ Board.Empty)  = return $ drawBoard' b
drawBoard b@(Board.Board _ _ _ Board.Red)    = return $ pictures [color transRed $ rectangleSolid 700 600
                                                                 , drawBoard' b
                                                                 ]
drawBoard b@(Board.Board _ _ _ Board.Yellow) = return $ pictures [color transYellow $ rectangleSolid 700 600
                                                                 , drawBoard' b
                                                                 ]
drawBoard b@(Board.Board _ _ _ Board.Both)   = return $ pictures [ color transYellow $ rectangleSolid 700 600
                                                                 , color transRed $ rectangleUpperSolid 700 600
                                                                 , drawBoard' b
                                                                 ]
