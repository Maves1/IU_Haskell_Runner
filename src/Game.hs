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
    { manSpeedX      :: Float
    , manSpeedY      :: Float
    , gameSpeed      :: Float
    , gameState      :: GameState
    , manPosX        :: Float
    , manPosY        :: Float
    , backgrounds    :: [Float]
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
manPic :: Picture
manPic = unsafePerformIO . loadBMP . getSprite $ "man"

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
  | gameState game == Running = pictures [renderBackstage, renderPlayer, renderObstacles]
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
    renderObstacles =
      translate nextObstaclePos obstacleY obstaclePic

-- >>> initTranslateGrass
-- 674.0
-- | Physics constants
gAcc :: Float
gAcc = 600

jumpForce :: Float
jumpForce = 400

checkFloorCollision :: Game -> Bool
checkFloorCollision game = manPosY game < bottomBorder

updateGame :: Float -> Game -> Game
updateGame seconds game =
  game
    { manPosY = nextManPosY
    , manSpeedY = nextManSpeedY
    , backgroundPosX = nextBackgroundPosX
    , backgrounds = nextBackgrounds
    , obstacles = nextObstacles
    , obstaclesTranslation = nextObstaclesTranslation
    }
  where
    nextManPosY
      | checkFloorCollision game = bottomBorder
      | otherwise = manPosY game + manSpeedY game * seconds
    nextManSpeedY
      | checkFloorCollision game = 0
      | otherwise = manSpeedY game - gAcc * seconds
    nextBackgroundPosX = backgroundPosX game - 4
    nextBackgrounds
      | nextBackgroundPosX < -(head $ tail $ backgrounds game) =
        drop 1 $ backgrounds game
      | otherwise = backgrounds game
    nextObstaclesTranslation
      | obstaclesTranslation game < - windowSizeX / 2 - 100 - head (obstacles game) = 0
      | otherwise = obstaclesTranslation game - 4
    nextObstacles
      | nextObstaclesTranslation == 0 = drop 1 $ obstacles game
      | otherwise = obstacles game

-- testGame :: Game
-- testGame = initGame{backgroundPosX = -grassWidth, backgrounds = take 5 $ backgrounds initGame}
-- >>> updateGame 0 testGame
-- Game {manSpeedX = 0.0, manSpeedY = 0.0, gameSpeed = 0.0, gameState = Running, manPosX = -100.0, manPosY = -190.0, backgrounds = [2048.0,4096.0,6144.0,8192.0], backgroundPosX = -2052.0}
initGame :: StdGen -> Game
initGame g =
  Game
    { manSpeedX = 0
    , manSpeedY = 0
    , gameSpeed = 0
    , gameState = Running
    , manPosX = -100
    , manPosY = bottomBorder
    , backgrounds = [0,grassWidth ..]
    , backgroundPosX = 0
    , obstacles = randomRs (350, 1000) g
    , obstaclesTranslation = 1
    }

handleEvents :: Event -> Game -> Game
handleEvents (EventKey (SpecialKey KeySpace) Down _ _) game =
  game {manSpeedY = newManSpeedY}
  where
    newManSpeedY
      | manPosY game == bottomBorder = jumpForce
      | otherwise = manSpeedY game
handleEvents _ game = game

runGame :: IO ()
runGame = do
  g <- newStdGen
  play window white 60 (initGame g) render handleEvents updateGame
-- runGame :: IO ()
-- runGame = display window white (render initGame)
