module MyFB where

import           Debug.Trace
import           Graphics.Gloss
import           Graphics.Gloss.Data.Bitmap
import           Graphics.Gloss.Interface.IO.Game
import           System.IO.Unsafe
import           System.Random

-- ============================ GAME CONSTANTS ============================
fps :: Int
fps = 60

posWindow :: (Int, Int)
posWindow = (50, 50)

birdSize :: Float
birdSize = 18

barSize :: Float
barSize = 75

sizeXWindow :: Float
sizeXWindow = 600

sizeYWindow :: Float
sizeYWindow = 800

speedBar :: Float
speedBar = -200

speedBird :: Float
speedBird = 450

speedDelta :: Float
speedDelta = -15

speedGravity :: Float
speedGravity = 600

barWidth :: Float
barWidth = 8

title :: String
title = "Flappy Bird in Haskell!!!"

background :: Color
background = white

-- ============================ GET GAME SPRITES ============================
getSprite :: String -> FilePath
getSprite name = "sprites/" ++ name ++ ".bmp"

bg0Pic :: Picture
bg0Pic =
  translate (1024 - sizeXWindow / 2) 0 . unsafePerformIO . loadBMP . getSprite $
  "bg0"

bg1Pic :: Picture
bg1Pic =
  translate (1024 - sizeXWindow / 2) 0 . unsafePerformIO . loadBMP . getSprite $
  "bg1"

birdAlive :: Picture
birdAlive = scale 0.5 0.5 . unsafePerformIO . loadBMP . getSprite $ "bird0"

birdDead :: Picture
birdDead = scale 0.5 0.5 . unsafePerformIO . loadBMP . getSprite $ "bird1"

botBar :: Picture
botBar = scale 2 2 . unsafePerformIO . loadBMP . getSprite $ "bot"

fbPic :: Picture
fbPic =
  translate 0 200 . scale 3 3 . unsafePerformIO . loadBMP . getSprite $ "fb"

gameOver :: Picture
gameOver = scale 3 3 . unsafePerformIO . loadBMP . getSprite $ "go"

pausePic :: Picture
pausePic = scale 5 5 . unsafePerformIO . loadBMP . getSprite $ "pause"

tapPic :: Picture
tapPic =
  translate (-130) (-55) . scale 2 2 . unsafePerformIO . loadBMP . getSprite $
  "tap"

topBar :: Picture
topBar = scale 2 2 . unsafePerformIO . loadBMP . getSprite $ "top"

groundBgPicture :: Float -> Picture
groundBgPicture x = translate x 0 bg0Pic

cloudsBgPicture :: Float -> Picture
cloudsBgPicture x = translate x 0 bg1Pic

barPicture :: Float -> Float -> Picture
barPicture x y = translate x (-y) displayBar

birdPicture :: Float -> Picture
birdPicture y = translate 0 y birdAlive

-- ============================================================================
-- Displayed window of the game
window :: Display
window = InWindow title (round sizeXWindow, round sizeYWindow) posWindow

data BirdStatus
  = AliveBird
  | DeadBird
  deriving (Show, Eq)

data GameStatus
  = Start
  | Normal
  | Pause
  | Dead
  | Cheat
  deriving (Show, Eq)

data GameState =
  FbGame
    { birdPos    :: Float
    , birdSpeed  :: Float
    , birdStatus :: BirdStatus
    , barHeight  :: Float
    , barPos     :: Float
    , barSpeed   :: Float
    , bg0Pos     :: Float
    , bg1Pos     :: Float
    , rds        :: [Float]
    , score      :: Int
    , gameStatus :: GameStatus
    }
  deriving (Show)

initialState :: [Float] -> GameState
initialState fs =
  FbGame
    { birdPos = 0
    , birdSpeed = 0
    , birdStatus = AliveBird
    , barHeight = 0
    , barPos = 0
    , barSpeed = speedBar
    , bg0Pos = 0
    , bg1Pos = 0
    , rds = fs
    , score = 0
    , gameStatus = Start
    }

-- ============================ GAME LOGIC ============================
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeySpace) Down _ _) game =
  case gameStatus game of
    Start  -> game {gameStatus = Normal}
    Normal -> game {birdSpeed = speedBird}
    Pause  -> game {gameStatus = Normal}
    Dead   -> initialState (rds game)
    Cheat  -> game {gameStatus = Normal}
handleInput (EventKey (Char 'r') Down _ _) game = initialState (rds game)
handleInput _ game = game

moveBg :: Float -> GameState -> GameState
moveBg sec game = game {bg0Pos = p0', bg1Pos = p1'}
  where
    (p0, p1, v) = (bg0Pos game, bg1Pos game, barSpeed game)
    p0'
      | p0 > (-2048) = p0 + (v * 0.1) * sec
      | otherwise = p0 + (v * 0.1) * sec + 2048
    p1'
      | p1 > (-1024) = p1 + (v - 100) * sec
      | otherwise = p1 + (v - 100) * sec + 1024

displayBar :: Picture
displayBar = translate (sizeXWindow / 2 + 26) (sizeYWindow / 2 - 850) botBar

moveBar :: Float -> GameState -> GameState
moveBar sec game =
  game {barPos = p', barHeight = s', barSpeed = v', rds = r', score = sc'}
  where
    (p, s, v, r, sc) =
      (barPos game, barHeight game, barSpeed game, rds game, score game)
    (p', s', v', r', sc') -- if p > -size_x_window then (p + speed_bar*sec , s, r) else (0,sNew,rNew)
      | p > -(sizeXWindow + barWidth * 3) =
        (p + v * sec, s, v + speedDelta * sec, r, sc)
      | otherwise = (0, sNew, v + speedDelta * sec, rNew, sc + 1)
    sNew = head r
    rNew = tail r

movePlayer :: Float -> GameState -> GameState
movePlayer sec game
  | gameStatus game == Cheat =
    game {birdPos = sizeYWindow / 2 - 100 - barHeight game + 50}
  | otherwise = game {birdPos = p', birdSpeed = v', birdStatus = s'}
  where
    (p, v) = (birdPos game, birdSpeed game)
    (p', v', s') =
      ( max (-250) (p + v' * sec)
      , v - speedGravity * sec
      , if v' > 0
          then birdStatus game
          else birdStatus game)
        -- update velocity and position of the bird. Jump logic handles in this function

render :: GameState -> Picture
render game
  | gameStatus game == Start = pictures [standard, fbPic, tapPic]
  | gameStatus game == Pause = pictures [standard, pausePic]
  | gameStatus game == Dead = pictures [standard, gameOver]
  | otherwise = pictures [standard]
  where
    standard =
      pictures
        [ groundBgPicture (bg0Pos game)
        , barPicture (barPos game) (barHeight game)
        , cloudsBgPicture (bg1Pos game)
        , birdPicture (birdPos game)
          -- debug game,
        ]

run :: IO ()
run = do
  r <- getStdGen
  -- play ::  forall world.
  -- Display
  -- -> Color
  -- -> Int
  -- -> world
  -- -> (world -> Picture)
  -- -> (Event -> world -> world)
  -- -> (Float -> world -> world)
  -- -> IO ()
  play
    window
    background
    fps
    (initialState (randomRs ranTup r))
    render
    handleInput
    update
  where
    ranTup = (0, sizeYWindow - 500 - 300)
    update :: Float -> GameState -> GameState
    update sec game
      | gameStatus game == Normal || gameStatus game == Cheat
       = moveBg sec . moveBar sec . movePlayer sec $ game
      | otherwise = game
