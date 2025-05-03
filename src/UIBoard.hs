module UIBoard (drawBoard) where

import Data.Map.Strict ((!))
import Graphics.Gloss

import qualified Board

--------------------------------
-- Basic coin and color setup --
--------------------------------

gameCoin :: Picture
gameCoin = circleSolid 40

transRed, transYellow :: Color
transRed    = makeColorI 255   0   0 128
transYellow = makeColorI 255 255   0 128

-- Define neon yellow for winner text when Yellow wins.
neonYellow :: Color
neonYellow = makeColorI 255 255 0 255

convColor :: Board.Color -> (Picture -> Picture)
convColor Board.Empty  = color white
convColor Board.Red    = color red
convColor Board.Yellow = color yellow

--------------------------------
-- Positions and coin drawing --
--------------------------------

manipX :: Int -> Float
manipX x = fromIntegral (x - 4) * 100

manipY :: Int -> Float
manipY y = fromIntegral (2 * y - 7) * 50

drawCoin :: Int -> Int -> Board.Color -> Picture
drawCoin x y col =
  let coinPic = convColor col gameCoin
      -- Optional shadow for 3D effect
      shadow  = translate 5 (-5) $
                  color (makeColor 0 0 0 0.5) $
                    circleSolid 45
      coinPos = translate (manipX x) (manipY y)
  in coinPos (pictures [shadow, coinPic])

drawBoard' :: Board.Board -> Picture
drawBoard' b =
  let bMap = Board.board b
  in pictures
       [ drawCoin x y (bMap ! (x, y))
       | x <- [1 .. Board.columns]
       , y <- [1 .. Board.rows]
       ]

--------------------------------
-- Background and winner text --
--------------------------------

boardBackground :: Picture
boardBackground =
  pictures
    [ color (makeColor 0.1 0.5 0.3 1) (rectangleSolid 800 700)
    , color (makeColor 0   0.3 0.2 0.3) (rectangleSolid 800 700)
    ]

-- | Helper function that layers a picture (in this case, text) in eight directions around it.
simulateBold :: Float -> Picture -> Picture
simulateBold off pic =
  pictures $ [ translate (off * cos a) (off * sin a) pic | a <- angles ] ++ [pic]
  where angles = [0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4]

-- | Draw the winner message aligned to the left with a rectangle behind it.
-- | Draw the winner message aligned to the left with a rectangle behind it.
winnerText :: Board.Board -> Picture
winnerText b =
  let w       = Board.winner b
      message = case w of
                  Board.Red    -> "Red Wins!"
                  Board.Yellow -> "Yellow Wins!"
                  Board.Both   -> "It's a Draw!"
                  Board.Empty  -> ""
      scaleFactor = 0.4
      -- Choose the text color based on the winning player:
      textColor = case w of
                    Board.Red    -> red
                    Board.Yellow -> neonYellow
                    _            -> white
      baseText   = color textColor $ scale scaleFactor scaleFactor $ Text message
      boldOffset = 2
      boldText   = simulateBold boldOffset baseText
      textWidth  = fromIntegral (length message) * 10 * scaleFactor
      padding    = 600
      rectWidth  = textWidth + padding
      rectHeight = 100  -- Adjust the height if desired.
      yOffset    = 200
      margin     = 190  -- margin from the left edge of the background rectangle

      -- Background rectangle color based on winner
      bgColor = case w of 
                  Board.Red    -> makeColor 1 0.8 0.8 0.8     -- very light red
                  Board.Yellow -> makeColor 0 0.4 0 0.8       -- little bit lighter green
                  _            -> makeColor 0 0.3 0 0.8       -- default green
      bgRect = color bgColor $ rectangleSolid rectWidth rectHeight

  in if null message
     then Blank
     else 
       translate 0 yOffset $
         pictures 
           [ bgRect
           , translate (-rectWidth/2 + margin) (-10) boldText
           ]


--------------------------------
-- Top-level draw function(s) --
--------------------------------

drawBoard :: Board.Board -> IO Picture
drawBoard b@(Board.Board _ _ _ Board.Empty) =
  -- Game in progress: just show background + coins
  return $ pictures
    [ boardBackground
    , drawBoard' b
    ]

drawBoard b@(Board.Board _ _ _ Board.Red) =
  -- Red has won: draw background, coins, then the red overlay,
  -- and finally the winner text on top.
  return $ pictures
    [ boardBackground
    , drawBoard' b
    , color transRed $ rectangleSolid 700 600
    , winnerText b
    ]

drawBoard b@(Board.Board _ _ _ Board.Yellow) =
  -- Yellow has won: draw background, coins, then the yellow overlay,
  -- and finally the winner text on top.
  return $ pictures
    [ boardBackground
    , drawBoard' b
    , color transYellow $ rectangleSolid 700 600
    , winnerText b
    ]

drawBoard b@(Board.Board _ _ _ Board.Both) =
  -- For a draw or some combined state: draw background, coins,
  -- then both overlays (the order here can be adjusted as needed),
  -- and finally the winner text on top.
  return $ pictures
    [ boardBackground
    , drawBoard' b
    , color transYellow $ rectangleSolid 700 600
    , color transRed $ rectangleUpperSolid 700 600
    , winnerText b
    ]
