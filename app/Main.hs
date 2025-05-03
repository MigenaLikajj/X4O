
import System.Exit

import Graphics.Gloss hiding (color)
import Graphics.Gloss.Interface.IO.Game hiding (color)

import AI (GamePosition)
import Board hiding (Color)
import UIBoard (drawBoard)
import Player

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