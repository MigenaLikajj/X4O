# X4O Game

This is an implementation of the X4O game in Haskell, where players can choose between three different states: Human, Easy Player, and Hard Player. The game integrates the negamax algorithm to determine the next best move for the AI players.

## How to Play

To start the game, run the `Main.hs` file. You will be prompted to choose the type of player for Player 1 and Player 2. The available choices are:

1. Human Player: A human player who will make moves interactively.
2. Easy Player: An AI player that makes simple and less strategic moves.
3. Hard Player: An AI player that uses the negamax algorithm to make more strategic moves.

Enter the corresponding number for each player to select the desired player type.

The game is played on a 6x7 grid. Player 1 uses the Red color, and Player 2 uses the Yellow color. To make a move, click on the desired column in the game window. The move will be applied according to the rules of X4O.

## Dependencies

The game uses the `gloss` package for the user interface.

## Running the Game

To run the game, make sure you have the Haskell compiler and the `gloss` package installed. Then, compile and run the `Main.hs` file using the following command:

stack run

## Credits

The implementation of the X4O game and the negamax algorithm are provided by the following modules:

- `Board.hs`: Defines the game board and the functions to manipulate it.
- `AI.hs`: Implements the negamax algorithm for choosing the next best move.
- `UIBoard.hs`: Handles the drawing of the game board using the `gloss` package.
- `Player.hs`: Defines the player types and their behaviors.

Feel free to explore and modify the code to enhance the game or customize it according to your needs.
