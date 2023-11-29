{-# OPTIONS_GHC -Wall #-}

module Game
  ( runGame
  ) where

import           Graphics.Gloss
import           Graphics.Gloss.Data.Bitmap       ()
import           Graphics.Gloss.Interface.IO.Game
import           System.IO.Unsafe
import qualified System.Random                    as R

-- Run: stack ghc -- Game.hs -o main -threaded
-- | Game data types
data GameState
  = Welcome -- | Welcome screen
  | Running -- | Game is running
  | Paused -- | Game is paused
  | Over -- | Game is over
  | Finished -- | Game is finished
  deriving (Show, Eq)

-- | Obstacle types
data ObstacleType
  = Zombie
  | Bird
  deriving (Show, Eq)

-- | Obstacle data type with position and type
data Obstacle =
  Obstacle
    { obstacleType   :: ObstacleType
    , obstaclePos    :: Float
    , obstacleY      :: Float
    , yMoveDirection :: Bool -- | True - up, False - down, used only for birds
    }
  deriving (Show)

-- | data type for player with position and speed
data Man =
  Man
    { speedX :: Float
    , speedY :: Float
    , posX   :: Float
    , posY   :: Float
    }
  deriving (Show)

-- | Data type for game
data Game =
  Game
    { man                  :: Man -- | Player
    , gameSpeed            :: Float -- | Game speed, increases with time
    , gameState            :: GameState -- | Current game state
    , backgrounds          :: [Float] -- | List of background positions
    , backgroundPosX       :: Float -- | Current background position
    , obstacles            :: [Obstacle] -- | List of obstacles
    , obstaclesTranslation :: Float -- | Current obstacles translation
    , generator            :: R.StdGen -- | Random generator
    , gameScore            :: Int -- | Current game score
    }
  deriving (Show)

-- ================== CONSTANTS ==================
title :: String
title = "Идущий к Реке v1.0 beta"

-- | Window size in pixels
windowSizeX :: Float
windowSizeX = 700

windowSizeY :: Float
windowSizeY = 700

-- | size of the player sprite in pixels
manSize :: Float
manSize = 16

-- | size of the grass sprite in pixels
grassSize :: Float
grassSize = 150

-- | size of the grass under the earth in pixels
topGrassSize :: Float
topGrassSize = 22

-- | width of the grass sprite in pixels
grassWidth :: Float
grassWidth = 2048

-- | distance from the bottom border of the window to the bottom border of the grass sprite
bottomBorder :: Float
bottomBorder = -windowSizeY / 2 + grassSize

-- | width of the zombie sprite in pixels
zombieWidth :: Float
zombieWidth = 28

-- | height of the zombie sprite in pixels
zombieHeight :: Float
zombieHeight = 64

-- | Distance from the bottom border of the window
--   to the upper border of the zombie sprite
zombieY :: Float
zombieY = bottomBorder + zombieHeight / 2 - topGrassSize -- | topGrassSize accounts for top-most layer of grass

-- | height of the bird sprite in pixels
birdHeight :: Float
birdHeight = 16

-- | width of the bird sprite in pixels
birdWidth :: Float
birdWidth = 16

-- | Bird can vertically move between birdYMin and birdYMax
-- and Y is counted from the bottom border of the window
birdYMax :: Float
birdYMax = bottomBorder + birdHeight / 2 - topGrassSize + 100

birdYMin :: Float
birdYMin = bottomBorder + birdHeight / 2 - topGrassSize

scoreFinalOffsetY :: Float
scoreFinalOffsetY = -170

welcomePicOffsetY :: Float
welcomePicOffsetY = 175

gameOverPicOffsetY :: Float
gameOverPicOffsetY = 175

-- | Load sprites
zombiePic :: Picture
{-# NOINLINE zombiePic #-}
zombiePic = unsafePerformIO . loadBMP . getSprite $ "zombie"

pic2bmp :: Picture -> BitmapData
pic2bmp (Bitmap bmpData) = bmpData
pic2bmp _                = error "pic2bmp: not a bitmap"

numsSprite :: BitmapData
numsSprite = pic2bmp nums

nums :: Picture
{-# NOINLINE nums #-}
nums = unsafePerformIO . loadBMP . getSprite $ "num"

getSprite :: String -> FilePath
getSprite name = "sprites/" ++ name ++ ".bmp"

manPic :: Picture
{-# NOINLINE manPic #-}
manPic = unsafePerformIO . loadBMP . getSprite $ "man"

grassPic :: Picture
grassPic =
  translate
    0
    initTranslateGrassY
    (unsafePerformIO . loadBMP . getSprite $ "grass")

skyPic :: Picture
{-# NOINLINE skyPic #-}
skyPic = unsafePerformIO . loadBMP . getSprite $ "sky"

welcomePic :: Picture
{-# NOINLINE welcomePic #-}
welcomePic = unsafePerformIO . loadBMP . getSprite $ "welcome_sign"

gameOverPic :: Picture
{-# NOINLINE gameOverPic #-}
gameOverPic = unsafePerformIO . loadBMP . getSprite $ "game_over_sign"

birdPic :: Picture
{-# NOINLINE birdPic #-}
birdPic = unsafePerformIO . loadBMP . getSprite $ "monster"

-- | Window position in pixels
windowPosition :: (Int, Int)
windowPosition = (100, 100)

-- | Translate grass sprite to the bottom border of the window
initTranslateGrassX :: Float
initTranslateGrassX = grassWidth / 2 - windowSizeX / 2 -- starting from the left border of the picture

initTranslateGrassY :: Float
initTranslateGrassY = -windowSizeY / 2 + grassSize / 2

-- | Graphics helper functions
window :: Display
window = InWindow title (round windowSizeX, round windowSizeY) windowPosition

-- | Acceleration of the game speed
accelerate :: Float
accelerate = 0.002

-- | Acceleration of the gravity
gAcc :: Float
gAcc = 600

jumpForce :: Float
jumpForce = 400

-- ===============================================
-- | Get current obstacle picture
getCurObstaclePic :: Obstacle -> Picture
getCurObstaclePic obstacle =
  case obstacleType obstacle of
    Zombie -> zombiePic
    Bird   -> birdPic

-- | render the background of the game (sky and grass)
renderBackstage :: Game -> Picture
renderBackstage game =
  translate (translationGrassX curBackstagePos) 0 backstage <>
  translate (translationGrassX nextBackstagePos) 0 backstage
  where
    backstage = pictures [skyPic, grassPic]
    curBackstagePos = head $ backgrounds game
    nextBackstagePos = head $ tail $ backgrounds game
    translationGrassX :: Float -> Float -- | translate grass and sky sprite to the given position
    translationGrassX x = initTranslateGrassX + backgroundPosX game + x

-- | Reneder the game
render :: Game -> Picture
render game
  | gameState game == Welcome =
    pictures [renderBackstage game, renderPlayer, renderWelcome]
  | gameState game == Running =
    pictures [renderBackstage game, renderPlayer, renderObstacles, renderScore]
  | gameState game == Over =
    pictures [renderBackstage game, renderOver, translate 0 scoreFinalOffsetY renderScore]
  | otherwise = pictures [renderBackstage game]
  where
    renderPlayer = translate (posX $ man game) (posY $ man game) manPic
    nextObstaclePos =
      obstaclePos (head (obstacles game)) + obstaclesTranslation game
    renderObstacles =
      translate nextObstaclePos (obstacleY (head (obstacles game))) $
      getCurObstaclePic (head (obstacles game))
    renderScore = scoreGen (gameScore game)
    renderWelcome = translate 0 welcomePicOffsetY welcomePic
    renderOver = translate 0 gameOverPicOffsetY gameOverPic

-- | Generate score picture from 0 to 99
scoreGen :: Int -> Picture
scoreGen int =
  translate (-0) 250 $ scale 3 3 $ pictures [translate (-8) 0 $ dig d, dig u]
  where
    dig i = bitmapSection (Rectangle (7 * i, 0) (7, 10)) numsSprite
    u = int `mod` 10
    d = int `div` 10

-- | Check if player is on the floor
checkFloorCollision :: Game -> Bool
checkFloorCollision game = posY (man game) < bottomBorder

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

-- | Check if player crushes with the obstacle
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
      (obstacleY curObstacle + getObstacleHeight curObstacle / 2 + topGrassSize) &&
      playerPosY >= (obstacleY curObstacle - getObstacleHeight curObstacle / 2)

-- | Update game state
updateGameSate :: Game -> GameState
updateGameSate game
  | gameState game == Over = Over
  | checkCrush game = Over
  | otherwise = gameState game

-- | Update bird vertical position
updateObstacleY :: Obstacle -> Obstacle
updateObstacleY obstacle
  | obstacleType obstacle == Zombie = obstacle
  | otherwise = obstacle {obstacleY = newY, yMoveDirection = newMoveDirection}
  where
    newY =
      if yMoveDirection obstacle
        then obstacleY obstacle + 1
        else obstacleY obstacle - 1
    newMoveDirection =
      if newY >= birdYMax || newY <= birdYMin
        then not $ yMoveDirection obstacle
        else yMoveDirection obstacle

-- | Update player position and speed
updateMan :: Float -> Game -> Man
updateMan seconds game = (man game) {posY = nextManPosY, speedY = nextManSpeedY}
  where
    nextManPosY
      | checkFloorCollision game = bottomBorder
      | otherwise = posY (man game) + speedY (man game) * seconds
    nextManSpeedY
      | speedY (man game) == 0 = 0
      | checkFloorCollision game = 0
      | otherwise = speedY (man game) - gAcc * seconds

-- | Update game score after passing the obstacle
updateScore :: Game -> Int
updateScore game =
  case gameState game of
    Over -> gameScore game
    Welcome -> gameScore game
    _ ->
      if obstaclesTranslation game == 0
        then gameScore game + 1
        else gameScore game

-- | Update background positions
updateBackgounds :: Game -> (Float, [Float])
updateBackgounds game = (nextBackgroundPosX, nextBackgrounds)
  where
    nextBackgroundPosX = backgroundPosX game - gameSpeed game
    nextBackgrounds
      | nextBackgroundPosX < -(head $ tail $ backgrounds game) =
        drop 1 $ backgrounds game
      | otherwise = backgrounds game

-- | Update obstacles positions
updateObstacles :: Game -> (Float, [Obstacle])
updateObstacles game = (nextObstaclesTranslation, nextObstacles)
  where
    nextObstaclesTranslation
      | obstaclesTranslation game <
          -windowSizeX / 2 - obstaclePos (head (obstacles game)) = 0
      | otherwise = obstaclesTranslation game - gameSpeed game
    nextObstacles
      | nextObstaclesTranslation == 0 = drop 1 $ obstacles game
      | otherwise = map updateObstacleY $ obstacles game

-- | Update the total game state
updateGame :: Float -> Game -> Game
updateGame seconds game =
  case gameState game of
    Welcome -> game
    Over -> game
    _ ->
      game
        { man = updateMan seconds game
        , backgroundPosX = nextBackgroundPosX
        , backgrounds = nextBackgrounds
        , obstacles = nextObstacles
        , obstaclesTranslation = nextObstaclesTranslation
        , gameState = updateGameSate game
        , gameSpeed = gameSpeed game + accelerate
        , gameScore = updateScore game
        }
  where
    nextBackgroundPosX = fst $ updateBackgounds game
    nextBackgrounds = snd $ updateBackgounds game
    nextObstaclesTranslation = fst $ updateObstacles game
    nextObstacles = snd $ updateObstacles game

-- | Generate random obstacle with random tyoe and position
generateObstacle :: R.StdGen -> Obstacle
generateObstacle g =
  Obstacle
    { obstacleType = obsType
    , obstaclePos = fst $ R.randomR (350, 1000) g
    , obstacleY =
        if obsType == Zombie
          then zombieY
          else fst $ R.randomR (birdYMin, birdYMax) g
    , yMoveDirection = fst $ R.randomR (True, False) g
    }
  where
    minCat = 0 :: Int
    maxCat = 1 :: Int
    obsType =
      if fst (R.randomR (minCat, maxCat) g) == 0
        then Zombie
        else Bird

-- | Generate list of obstacles
generateObstacles :: R.StdGen -> [Obstacle]
generateObstacles g = generateObstacle g : generateObstacles (snd $ R.split g)

-- | Initialize the game
initGame :: R.StdGen -> Game
initGame g =
  Game
    { man = Man {speedX = 0, speedY = 0, posX = -100, posY = bottomBorder}
    , gameSpeed = 4
    , gameState = Welcome
    , backgrounds = [0,grassWidth ..]
    , backgroundPosX = 0
    , obstacles = generateObstacles g
    , obstaclesTranslation = 1
    , generator = g
    , gameScore = 0
    }

-- | Handle events
--  Space - jump
--  R - restart
handleEvents :: Event -> Game -> Game
handleEvents (EventKey (SpecialKey KeySpace) Down _ _) game =
  case gameState game of
    Welcome -> game {gameState = Running}
    Running -> game {man = (man game) {speedY = newManSpeedY}}
    _       -> game
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
