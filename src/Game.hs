module Game(runGame) where

import Debug.Trace
import System.Random
import System.IO.Unsafe

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap ()
import Graphics.Gloss.Interface.IO.Game

-- Run: stack ghc -- Game.hs -o main -threaded


data GameState = Welcome | Running | Paused | Over | Finished deriving (Show, Eq)

-- We need to store game state
data Game = Game {
    manSpeedX :: Float,
    manSpeedY :: Float,
    gameSpeed :: Float,
    gameState :: GameState
} deriving Show

title :: String
title = "Идущий к Реке v1.0 pre-alpha"

windowSizeX :: Float
windowSizeX = 350

windowSizeY :: Float
windowSizeY = 200

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
grassPic = unsafePerformIO . loadBMP . getSprite $ "grass"

skyPic :: Picture
skyPic = unsafePerformIO . loadBMP . getSprite $ "sky"

welcomePic :: Picture
welcomePic = unsafePerformIO . loadBMP . getSprite $ "sky"

render :: Game -> Picture
render game
    | gameState game == Welcome = pictures [backstage, welcomePic]
    | gameState game == Running = pictures [manPic]
    | otherwise = pictures [backstage]
    where
        backstage = pictures [
            grassPic]

-- | Physics constants

gAcc :: Double
gAcc = 9.8

jumpForce :: Double
jumpForce = 20

initGame :: Game
initGame = Game {
    manSpeedX = 0,
    manSpeedY = 0,
    gameSpeed = 0,
    gameState = Welcome
}

runGame :: IO ()
runGame = display window white (render initGame)
    
