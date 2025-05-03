
import System.Exit

import Graphics.Gloss hiding (color)
import Graphics.Gloss.Interface.IO.Game hiding (color)

import AI (GamePosition)
import Board hiding (Color)
import UIBoard (drawBoard)
import Player



moveFunc :: (Player Column Board) -> (Player Column Board) -> Event -> Board -> IO Board
moveFunc player1 player2 (EventKey (MouseButton LeftButton) Up _ (coordX, _)) b@Board{ winner = Empty } = let col = ceiling $ (coordX + 350) / 100
                                                                                                              moves = possibleMoves b
                                                                                                          in if null moves
                                                                                                             then return b { winner = Both }  -- draw
                                                                                                             else moveFunc' moves player1 player2 b col
moveFunc _ _ (EventKey (MouseButton LeftButton) Up _ _) _ = exitSuccess
moveFunc _ _ (EventKey (SpecialKey KeyEsc) Up _ _) _      = die "Game Aborted"
moveFunc _ _ (EventKey (Char 'q') Up _ _) _               = die "Game Aborted"
moveFunc _ _ _ b                                          = return b

moveFunc' moves player1 player2 b col =
    do
        let player = if (color b == Red) then player1 else player2
        move <- player moves b (Just col)
        let nBoard = makeMove b move
            hasWon = checkWin nBoard
        if hasWon
        then return nBoard { winner = color b }
        else return nBoard

timeFunc :: Float -> Board -> IO Board
timeFunc _ b = return b

window :: Display
window = InWindow "X4O !!" (700, 600) (10, 10)

background :: Color
background = dark blue


choosePlayer :: GamePosition b => Int -> IO (Player a b)
choosePlayer i = do putStrLn $ "Choose Player " ++ (show i) ++ ":"
                    putStrLn "1. Human Player"
                    putStrLn "2. Easy Player"
                    putStrLn "3. Hard Player"
                    putStrLn "Enter Choice: "
                    choiceString <- getLine
                    let choice = read choiceString
                    case choice of
                      1 -> return humanPlayer
                      2 -> return (negmaxplayer 1)
                      3 -> return (negmaxplayer 5)
                      _ -> do putStrLn $ "You have entered an invalid choice: " ++ (show choice)
                              putStrLn "Please choose one of the possible choices."
                              choosePlayer i

begin :: IO ()
begin = do
  player1 <- choosePlayer 1
  player2 <- choosePlayer 2
  playIO window background 1 initialBoard drawBoard (moveFunc player1 player2) timeFunc

main :: IO ()
main = begin