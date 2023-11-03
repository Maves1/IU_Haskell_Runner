module Game(runGame) where

import Debug.Trace
import System.Random
import System.IO.Unsafe

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap ()
import Graphics.Gloss.Interface.IO.Game

-- Run: stack ghc -- flappy_bird_example.hs -o main -threaded


data GameState = Welcome | Running | Paused | Over | Finished deriving Show

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

-- render :: Game -> Picture
-- render game
--     | gameState game == Welcome = pictures [backstage, welcomePic]
--     | gameState game == Running = pictures [standard, manPic]
--     | otherwise = pictures [backstage]
--     where
--         backstage = pictures [

--         ]

-- | Physics constants

gAcc :: Double
gAcc = 9.8

jumpForce :: Double
jumpForce = 20

runGame :: IO ()
runGame = do
    print "Hello, World!"
    
