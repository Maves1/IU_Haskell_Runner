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
    { manSpeedX :: Float
    , manSpeedY :: Float
    , gameSpeed :: Float
    , gameState :: GameState
    , manPosX   :: Float
    , manPosY   :: Float
    }
  deriving (Show)

title :: String
title = "Идущий к Реке v1.0 pre-alpha"

windowSizeX :: Float
windowSizeX = 700

windowSizeY :: Float
windowSizeY = 700

manSize :: Float
manSize = 128

grassSize :: Float
grassSize = 160

windowPosition :: (Int, Int)
windowPosition = (100, 100)

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

render :: Game -> Picture
render game
  | gameState game == Welcome = pictures [backstage]
  | gameState game == Running = pictures [backstage, renderPlayer]
  | otherwise = pictures [backstage]
  where
    renderPlayer = translate (manPosX game) (manPosY game) manPic
    backstage = pictures [skyPic, grassPic]

-- | Physics constants
gAcc :: Float
gAcc = 300

jumpForce :: Float
jumpForce = 300

initGame :: Game
initGame =
  Game
    { manSpeedX = 0
    , manSpeedY = 0
    , gameSpeed = 0
    , gameState = Running
    , manPosX = -100
    , manPosY = -windowSizeY / 2 + grassSize
    }

greenCircle :: Picture
greenCircle = color green $ circleSolid 10

handleEvents :: Event -> Game -> Game
handleEvents (EventKey (SpecialKey KeySpace) Down _ _) game =
  game {manSpeedY = jumpForce}
handleEvents _ game = game



runGame :: IO ()
runGame = do
  play window white 60 initGame render handleEvents update
  where
    update :: Float -> Game -> Game
    update seconds game =
      game
        { manPosY = manPosY game + manSpeedY game * seconds
        , manSpeedY = manSpeedY game - gAcc * seconds
        }