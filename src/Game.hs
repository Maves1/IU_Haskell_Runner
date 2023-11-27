{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Missing NOINLINE pragma" #-}
module Game
  ( runGame
  ) where

import           Graphics.Gloss
import           Graphics.Gloss.Data.Bitmap       ()
import           Graphics.Gloss.Interface.IO.Game
import           System.IO.Unsafe
import qualified System.Random                    as R

-- Run: stack ghc -- Game.hs -o main -threaded
data GameState
  = Welcome
  | Running
  | Paused
  | Over
  | Finished
  deriving (Show, Eq)

data ObstacleType
  = Zombie
  | Bird
  deriving (Show, Eq)

data Obstacle =
  Obstacle
    { obstacleType :: ObstacleType
    , obstaclePos  :: Float
    }
  deriving (Show)

data Man =
  Man
    { speedX :: Float
    , speedY :: Float
    , posX   :: Float
    , posY   :: Float
    }
  deriving (Show)

-- We need to store game state
data Game =
  Game
    { man                  :: Man
    , gameSpeed            :: Float
    , gameState            :: GameState
    , backgrounds          :: [Float]
    , backgroundPosX       :: Float
    , obstacles            :: [Obstacle]
    , obstaclesTranslation :: Float
    , generator           :: R.StdGen
    }
  deriving (Show)

title :: String
title = "Идущий к Реке v1.0 beta"

windowSizeX :: Float
windowSizeX = 700

windowSizeY :: Float
windowSizeY = 700

manSize :: Float
manSize = 16

topGrassSize :: Float
topGrassSize = 22

grassSize :: Float
grassSize = 150

grassWidth :: Float
grassWidth = 2048

skyWidth :: Float
skyWidth = 2048

bottomBorder :: Float
bottomBorder = -windowSizeY / 2 + grassSize

zombieWidth :: Float
zombieWidth = 28

zombieHeight :: Float
zombieHeight = 64

zombieY :: Float
zombieY = bottomBorder + zombieHeight / 2 - topGrassSize -- | topGrassSize accounts for top-most layer of grass

zombiePic :: Picture
-- obstaclePic = color red $ rectangleSolid obstacleWidth obstacleHeight
zombiePic = unsafePerformIO . loadBMP . getSprite $ "zombie"

birdHeight :: Float
birdHeight = 28

birdWidth :: Float
birdWidth = 64

birdPic :: Picture
birdPic = color red $ rectangleSolid birdWidth birdHeight

birdYMax :: Float
birdYMax = bottomBorder + birdHeight / 2 - topGrassSize + 100

birdYMin :: Float
birdYMin = bottomBorder + birdHeight / 2 - topGrassSize + 20

windowPosition :: (Int, Int)
windowPosition = (100, 100)

initTranslateGrassX :: Float
initTranslateGrassX = grassWidth / 2 - windowSizeX / 2 -- starting from the left border of the picture

initTranslateGrassY :: Float
initTranslateGrassY = -windowSizeY / 2 + grassSize / 2

initTranslateSky :: Float
initTranslateSky = skyWidth / 2 - windowSizeX / 2 -- starting from the left border of the picture

-- | Graphics helper functions
window :: Display
window = InWindow title (round windowSizeX, round windowSizeY) windowPosition

getSprite :: String -> FilePath
getSprite name = "sprites/" ++ name ++ ".bmp"

manPic :: Picture
manPic = unsafePerformIO . loadBMP . getSprite $ "man"

grassPic :: Picture
grassPic =
  translate
    0
    initTranslateGrassY
    (unsafePerformIO . loadBMP . getSprite $ "grass")

skyPic :: Picture
skyPic = unsafePerformIO . loadBMP . getSprite $ "sky"

welcomePic :: Picture
welcomePic = unsafePerformIO . loadBMP . getSprite $ "sky"

debugPic :: Float -> Float -> Picture
debugPic x y = translate x y $ color red $ circleSolid 5

getCurObstaclePic :: Obstacle -> Picture
getCurObstaclePic obstacle =
  case obstacleType obstacle of
    Zombie -> zombiePic
    Bird   -> birdPic

render :: Game -> Picture
render game
  | gameState game == Welcome = pictures [renderBackstage]
  | gameState game == Running =
    pictures [renderBackstage, renderPlayer, renderObstacles]
  | otherwise = pictures [renderBackstage]
  where
    renderPlayer = translate (posX $ man game) (posY $ man game) manPic
    backstage = pictures [skyPic, grassPic]
    curBackstagePos = head $ backgrounds game
    nextBackstagePos = head $ tail $ backgrounds game
    renderBackstage =
      translate
        (initTranslateGrassX + backgroundPosX game + curBackstagePos)
        0
        backstage <>
      translate
        (initTranslateGrassX + backgroundPosX game + nextBackstagePos)
        0
        backstage
    nextObstaclePos =
      obstaclePos (head (obstacles game)) + obstaclesTranslation game
    renderObstacles =
      translate nextObstaclePos (getObstacleY (head (obstacles game))) $
      getCurObstaclePic (head (obstacles game))

gAcc :: Float
gAcc = 600

jumpForce :: Float
jumpForce = 400

checkFloorCollision :: Game -> Bool
checkFloorCollision game = posY (man game) < bottomBorder

getObstacleY :: Obstacle -> Float
getObstacleY obstacle =
  case obstacleType obstacle of
    Zombie -> zombieY
    Bird   -> birdYMax

getObstacleHeight :: Obstacle -> Float
getObstacleHeight obstacle =
  case obstacleType obstacle of
    Zombie -> zombieHeight
    Bird   -> birdHeight

getObstacleWidth :: Obstacle -> Float
getObstacleWidth obstacle =
  case obstacleType obstacle of
    Zombie -> zombieWidth
    Bird   -> birdWidth

checkCrush :: Game -> Bool
checkCrush game =
  let playerPosX = posX $ man game
      playerPosY = posY $ man game
      curObstacle = head $ obstacles game :: Obstacle
      obstaclePosX = obstaclePos curObstacle + obstaclesTranslation game
   in (playerPosX + manSize / 2) >=
      (obstaclePosX - getObstacleWidth curObstacle / 2) &&
      playerPosX <= (obstaclePosX + getObstacleWidth curObstacle / 2) &&
      playerPosY <=
      (getObstacleY curObstacle + getObstacleHeight curObstacle / 2 +
       topGrassSize) &&
      playerPosY >=
      (getObstacleY curObstacle - getObstacleHeight curObstacle / 2)

updateGameSate :: Game -> GameState
updateGameSate game
  | gameState game == Over = Over
  | checkCrush game = Over
  | otherwise = gameState game

accelerate :: Float
accelerate = 0.01

updateGame :: Float -> Game -> Game
updateGame seconds game =
  game
    { man = (man game) {posY = nextManPosY, speedY = nextManSpeedY}
    , backgroundPosX = nextBackgroundPosX
    , backgrounds = nextBackgrounds
    , obstacles = nextObstacles
    , obstaclesTranslation = nextObstaclesTranslation
    , gameState = updateGameSate game
    , gameSpeed = gameSpeed game + accelerate
    }
  where
    nextManPosY
      | checkFloorCollision game = bottomBorder
      | otherwise = posY (man game) + speedY (man game) * seconds
    nextManSpeedY
      | speedY (man game) == 0 = 0
      | checkFloorCollision game = 0
      | otherwise = speedY (man game) - gAcc * seconds
    nextBackgroundPosX = backgroundPosX game - gameSpeed game
    nextBackgrounds
      | nextBackgroundPosX < -(head $ tail $ backgrounds game) =
        drop 1 $ backgrounds game
      | otherwise = backgrounds game
    nextObstaclesTranslation
      | obstaclesTranslation game <
          -windowSizeX / 2 - obstaclePos (head (obstacles game)) = 0
      | otherwise = obstaclesTranslation game - gameSpeed game
    nextObstacles
      | nextObstaclesTranslation == 0 = drop 1 $ obstacles game
      | otherwise = obstacles game

-- | Generate random obstacle with random tyoe and position
generateObstacle :: R.StdGen -> Obstacle
generateObstacle g =
  Obstacle
    { obstacleType =
        if fst (R.randomR (minCat, maxCat) g) == 0
          then Zombie
          else Bird
    , obstaclePos = fst $ R.randomR (350, 1000) g
    }
  where
    minCat = 0 :: Int
    maxCat = 1 :: Int

generateObstacles :: R.StdGen -> [Obstacle]
generateObstacles g = generateObstacle g : generateObstacles (snd $ R.split g)

initGame :: R.StdGen -> Game
initGame g =
  Game
    { man = Man {speedX = 0, speedY = 0, posX = -100, posY = bottomBorder}
    , gameSpeed = 4
    , gameState = Running
    , backgrounds = [0,grassWidth ..]
    , backgroundPosX = 0
    , obstacles = generateObstacles g
    , obstaclesTranslation = 1
    , generator = g
    }

handleEvents :: Event -> Game -> Game
handleEvents (EventKey (SpecialKey KeySpace) Down _ _) game =
  game {man = (man game) {speedY = newManSpeedY}}
  where
    newManSpeedY
      | posY (man game) == bottomBorder = jumpForce
      | otherwise = speedY (man game)
handleEvents (EventKey (Char 'r') Down _ _) _game = initGame (R.mkStdGen 0)
handleEvents _ game = game

runGame :: IO ()
runGame = do
  g <- R.newStdGen
  play window white 60 (initGame g) render handleEvents updateGame
