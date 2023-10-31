{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Project where

import CodeWorld
import System.Random

-- | Default entry point.
run :: IO ()
run = gameActivity

data Gate = Gate
  { width :: Double,
    offsetY :: Double,
    offsetX :: Double
  }
  deriving (Show)

sampleGates :: StdGen -> [Gate]
sampleGates g =
  zipWith3
    Gate
    widths
    ys
    xs
  where
    (g1, g2) = split g
    widths = randomRs (2.0, 3.0) g1
    ys = randomRs (-3.0, 3.0) g2
    xs = [0.0, 3.0 ..]

drawGate :: Gate -> Picture
drawGate (Gate w y x) = translated x y (solidRectangle w 1.0)

drawGates :: [Gate] -> Picture
drawGates = foldMap drawGate

data Player = Player
  { playerX :: Double,
    playerY :: Double
  }
  deriving (Show)

drawPlayer :: Player -> Picture
drawPlayer (Player x y) = translated x y (solidCircle 0.5)

data GameState = GameState
  { player :: Player,
    gates :: [Gate]
  }
  deriving (Show)

initialState :: StdGen -> GameState
initialState g = GameState (Player 0 0) (take 3 $ sampleGates g)

drawState :: GameState -> Picture
drawState (GameState p gs) = drawPlayer p <> translated 3 0 (drawGates gs)

collision :: Player -> Gate -> Bool
collision (Player px py) (Gate w gy gx) =
  let halfPlayerWidth = 0.5  -- Assuming player is a circle with radius 0.5
      halfPlayerHeight = 0.5  -- Assuming player is a circle with radius 0.5
      halfGateWidth = w / 2
      halfGateHeight = 0.5  -- Assuming gate height is always 1.0
      playerLeft = px - halfPlayerWidth
      playerRight = px + halfPlayerWidth
      playerTop = py + halfPlayerHeight
      playerBottom = py - halfPlayerHeight
      gateLeft = gx - halfGateWidth
      gateRight = gx + halfGateWidth
      gateTop = gy + halfGateHeight
      gateBottom = gy - halfGateHeight
  in not (playerLeft > gateRight || playerRight < gateLeft || playerTop < gateBottom || playerBottom > gateTop)


checkCollision :: Player -> [Gate] -> Bool
checkCollision p = any (collision p)

movePlayer :: Event -> Player -> Player
movePlayer (KeyPress "Up") (Player x y) = Player x (y + 0.2)
movePlayer (KeyPress "Down") (Player x y) = Player x (y - 0.2)
movePlayer (KeyPress "Left") (Player x y) = Player (x - 0.2) y
movePlayer (KeyPress "Right") (Player x y) = Player (x + 0.2) y
movePlayer _ p = p

handleEvent :: Event -> GameState -> GameState
handleEvent (TimePassing t) (GameState p gs) =
  if checkCollision p gs
    then GameState p gs
    else initialState (mkStdGen 0)
handleEvent e (GameState p gs) = GameState (movePlayer e p) gs

gameActivity :: IO ()
gameActivity = activityOf (initialState (mkStdGen 0)) handleEvent drawState
