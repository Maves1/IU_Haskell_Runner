module Game
  ( runGame
  ) where

import           Debug.Trace
import           System.IO.Unsafe
import           System.Random

import           Graphics.Gloss
import           Graphics.Gloss.Data.Bitmap       ()
import           Graphics.Gloss.Interface.IO.Game

-- Run: stack ghc -- Game.hs -o main -threaded
data GameState
  = Welcome
  | Running
  | Paused
  | Over
  | Finished
  deriving (Show, Eq)

-- We need to store game state
data Game =
  Game
    { manSpeedX            :: Float
    , manSpeedY            :: Float
    , gameSpeed            :: Float
    , gameState            :: GameState
    , manPosX              :: Float
    , manPosY              :: Float
    , backgrounds          :: [Float]
    , backgroundPosX       :: Float
    , obstacles            :: [Float]
    , obstaclesTranslation :: Float
    }
  deriving (Show)

title :: String
title = "Идущий к Реке v1.0 alpha"

windowSizeX :: Float
windowSizeX = 700

windowSizeY :: Float
windowSizeY = 700

manSize :: Float
manSize = 128

grassSize :: Float
grassSize = 160

grassWidth :: Float
grassWidth = 2048

skyWidth :: Float
skyWidth = 2048

bottomBorder :: Float
bottomBorder = -windowSizeY / 2 + grassSize

obstacleWidth :: Float
obstacleWidth = 30

obstacleHeight :: Float
obstacleHeight = 100

obstacleY :: Float
obstacleY = bottomBorder - 30

obstaclePic :: Picture
obstaclePic = color red $ rectangleSolid obstacleWidth obstacleHeight

windowPosition :: (Int, Int)
windowPosition = (100, 100)

initTranslateGrass :: Float
initTranslateGrass = grassWidth / 2 - windowSizeX / 2 -- starting from the left border of the picture

initTranslateSky :: Float
initTranslateSky = skyWidth / 2 - windowSizeX / 2 -- starting from the left border of the picture

-- | Graphics helper functions
window :: Display
window = InWindow title (round windowSizeX, round windowSizeY) windowPosition

getSprite :: String -> FilePath
getSprite name = "sprites/" ++ name ++ ".bmp"

-- backgroundPic :: Picture
-- backgroundPic =
-- manPic :: Picture
-- manPic = unsafePerformIO . loadBMP . getSprite $ "man"
manPic :: Picture
manPic = color black $ rectangleSolid manSize manSize

grassPic :: Picture
grassPic = unsafePerformIO . loadBMP . getSprite $ "bg1"

skyPic :: Picture
skyPic = unsafePerformIO . loadBMP . getSprite $ "sky"

welcomePic :: Picture
welcomePic = unsafePerformIO . loadBMP . getSprite $ "sky"

debugPic :: Float -> Float -> Picture
debugPic x y = translate x y $ color red $ circleSolid 5

render :: Game -> Picture
render game
  | gameState game == Welcome = pictures [renderBackstage]
  | gameState game == Running =
    pictures [renderBackstage, renderPlayer, renderObstacles]
  | otherwise = pictures [renderBackstage]
  where
    renderPlayer = translate (manPosX game) (manPosY game) manPic
    backstage = pictures [skyPic, grassPic]
    curBackstagePos = head $ backgrounds game
    nextBackstagePos = head $ tail $ backgrounds game
    renderBackstage =
      translate
        (initTranslateGrass + backgroundPosX game + curBackstagePos)
        0
        backstage <>
      translate
        (initTranslateGrass + backgroundPosX game + nextBackstagePos)
        0
        backstage
    nextObstaclePos = head (obstacles game) + obstaclesTranslation game
    renderObstacles = translate nextObstaclePos obstacleY obstaclePic

gAcc :: Float
gAcc = 600

jumpForce :: Float
jumpForce = 400

checkFloorCollision :: Game -> Bool
checkFloorCollision game = manPosY game < bottomBorder

checkCrush :: Game -> Bool
checkCrush game =
  let playerPosX = manPosX game
      playerPosY = manPosY game
      obstaclePosX = head (obstacles game) + obstaclesTranslation game
      obstaclePosY = obstacleY
   in (playerPosX + manSize / 2) >= (obstaclePosX -  obstacleWidth / 2) &&
      playerPosX <= (obstaclePosX + obstacleWidth / 2) &&
      playerPosY <= (obstaclePosY + obstacleHeight)

updateGameSate :: Game -> GameState
updateGameSate game
  | gameState game == Over = Over
  | checkCrush game = Over
  | otherwise = gameState game

updateGame :: Float -> Game -> Game
updateGame seconds game =
  game
    { manPosY = nextManPosY
    , manSpeedY = nextManSpeedY
    , backgroundPosX = nextBackgroundPosX
    , backgrounds = nextBackgrounds
    , obstacles = nextObstacles
    , obstaclesTranslation = nextObstaclesTranslation
    , gameState = updateGameSate game
    }
  where
    nextManPosY
      | checkFloorCollision game = bottomBorder
      | otherwise = manPosY game + manSpeedY game * seconds
    nextManSpeedY
      | manSpeedY game == 0 = 0
      | checkFloorCollision game = 0
      | otherwise = manSpeedY game - gAcc * seconds
    nextBackgroundPosX = backgroundPosX game - gameSpeed game
    nextBackgrounds
      | nextBackgroundPosX < -(head $ tail $ backgrounds game) =
        drop 1 $ backgrounds game
      | otherwise = backgrounds game
    nextObstaclesTranslation
      | obstaclesTranslation game <
          -windowSizeX / 2 - 100 - head (obstacles game) = 0
      | otherwise = obstaclesTranslation game - gameSpeed game
    nextObstacles
      | nextObstaclesTranslation == 0 = drop 1 $ obstacles game
      | otherwise = obstacles game

initGame :: StdGen -> Game
initGame g =
  Game
    { manSpeedX = 0
    , manSpeedY = 0
    , gameSpeed = 4
    , gameState = Running
    , manPosX = -100
    , manPosY = bottomBorder
    , backgrounds = [0,grassWidth ..]
    , backgroundPosX = 0
    , obstacles = randomRs (350, 1000) g
    , obstaclesTranslation = 1
    }

testGen :: StdGen
testGen = mkStdGen 0

testGame :: Game
testGame = initGame testGen

-- >>> checkFloorCollision testGame
-- False
handleEvents :: Event -> Game -> Game
handleEvents (EventKey (SpecialKey KeySpace) Down _ _) game =
  game {manSpeedY = newManSpeedY}
  where
    newManSpeedY
      | manPosY game == bottomBorder = jumpForce
      | otherwise = manSpeedY game
handleEvents (EventKey (Char 'r') Down _ _) game = initGame testGen
handleEvents _ game = game

runGame :: IO ()
runGame = do
  g <- newStdGen
  play window white 60 (initGame g) render handleEvents updateGame
