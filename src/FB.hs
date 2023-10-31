module FB where

import           Debug.Trace
import           Graphics.Gloss
import           Graphics.Gloss.Data.Bitmap
import           Graphics.Gloss.Interface.IO.Game
import           System.IO.Unsafe
import           System.Random

-- import Euterpea
-- ============================ GAME CONSTANTS ============================
colorBar :: Color
colorBar = green

colorBird :: Color
colorBird = red

forceUp :: Integer
forceUp = 10

forceDo :: Integer
forceDo = 3

fps :: Int
fps = 60

posWindow :: (Int, Int)
posWindow = (50, 50)

sizeXBar :: Integer
sizeXBar = 75

sizeXBart :: Integer
sizeXBart = 95

sizeYBart :: Integer
sizeYBart = 40

sizeABird :: Float
sizeABird = 18

sizeABars :: Float
sizeABars = 8

sizeASlit :: Float
sizeASlit = 200

sizeXWindow :: Float
sizeXWindow = 600

sizeYWindow :: Float
sizeYWindow = 800

speedBar :: Float
speedBar = -200

speedBird :: Float
speedBird = 300

speedDelta :: Float
speedDelta = -15

speedGravity :: Float
speedGravity = 600

title :: String
title = "Flappy Bird in Haskell!!!"

background :: Color
background = white

-- =================================================================
data GameState =
  FbGame
    { birdPos    :: Float
    , birdSpeed  :: Float
    , birdStatus :: Int
    , barSlit    :: Float -- space between gates
    , barPos     :: Float
    , barSpeed   :: Float
    , bg0Pos     :: Float
    , bg1Pos     :: Float
    , rds        :: [Float]
    , score      :: Int
    --  0:Start, 1:Normal, 2:Pause, 3:Dead, 4:Cheat
    , gameMode   :: Int
    }
  deriving (Show)

initialState :: [Float] -> GameState
initialState fs =
  FbGame
    { birdPos = 0
    , birdSpeed = 0
    , birdStatus = 0
    , barSlit = 0
    , barPos = 0
    , barSpeed = speedBar
    , bg0Pos = 0
    , bg1Pos = 0
    , rds = fs
    , score = 0
    , gameMode = 4
    }

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

-- ============================================================================
-- Displayed window of the game
window :: Display
window = InWindow title (round sizeXWindow, round sizeYWindow) posWindow

-- function for debug purpose
trace' :: (Show a) => a -> a
trace' a = trace (show a) a

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
    handleKeys
    update
    -- simulate window background fps (initialState (randomRs ranTup r)) render update
    -- animate window background $ frame $ initialState (randomRs ranTup r)
  where
    ranTup = (0, sizeYWindow - sizeASlit - 300)
    update :: Float -> GameState -> GameState
    update sec game
      | gameMode game == 1 || gameMode game == 4 =
        detectCrushBar .
        detectCrushFloor . moveBg sec . moveBar sec . movePlayer sec $
        game
      | otherwise = game

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) game
  | gameMode game == 0 = game {birdSpeed = speedBird, gameMode = 1}
  | gameMode game == 1 = game {birdSpeed = speedBird, gameMode = 1}
  | gameMode game == 2 = game {birdSpeed = speedBird, gameMode = 1}
  | gameMode game == 3 =
    (initialState (rds game)) {birdSpeed = speedBird, gameMode = 1}
  | gameMode game == 4 = game {birdSpeed = speedBird, gameMode = 1}
handleKeys (EventKey (Char 'r') Down _ _) game = initialState (rds game)
handleKeys (EventKey (Char 'l') Down _ _) game
  | gameMode game == 4 = game {gameMode = 1}
  | otherwise = game {gameMode = 4}
handleKeys (EventKey (Char 'p') Down _ _) game
  | gameMode game == 2 = game {gameMode = 1}
  | otherwise = game {gameMode = 2}
handleKeys _ game = game

detectCrushFloor :: GameState -> GameState
detectCrushFloor game
  | birdPos game < -250 = game {birdStatus = 3, gameMode = 3}
  | otherwise = game

detectCrushBar :: GameState -> GameState
detectCrushBar game
  | abs (barsP + (3 * sizeXWindow / 4)) < sizeABird + sizeABars &&
      (birdP + sizeABird > barsS + sizeYWindow / 2 - 100 ||
       birdP - sizeABird < barsS - sizeASlit + sizeYWindow / 2 - 100) =
    game {birdStatus = 4, gameMode = 3}
  | otherwise = game
  where
    birdP = birdPos game
    barsP = barPos game
    barsS = -barSlit game

debugger'' :: (Show a1, Show a2, Show a3) => (a1, a2, a3) -> [Char]
debugger'' (a, b, c) = show a ++ " - " ++ show b ++ " - " ++ show c

render :: GameState -> Picture
render game
  | gameMode game == 0 = pictures [standard, fbPic, tapPic]
  | gameMode game == 2 = pictures [standard, pausePic]
  | gameMode game == 3 = pictures [standard, gameOver]
  | otherwise = pictures [standard]
  where
    standard =
      pictures
        [ bg0 (bg0Pos game)
        , bar (barPos game) (barSlit game)
        , bg1 (bg1Pos game)
        , bird (birdPos game) (birdStatus game)
          -- debug game,
        , scoreGen (score game)
        ]

movePlayer :: Float -> GameState -> GameState
movePlayer sec game
  | gameMode game == 4 =
    game {birdPos = sizeYWindow / 2 - 100 - barSlit game - sizeASlit / 2}
  | otherwise = game {birdPos = p', birdSpeed = v', birdStatus = s'}
  where
    (p, v) = (birdPos game, birdSpeed game)
    (p', v', s') =
      ( p + v' * sec
      , v - speedGravity * sec
      , if v' > 0
          then 2
          else 1)
        -- update velocity and position of the bird. Jump logic handles in this function

moveBar :: Float -> GameState -> GameState
moveBar sec game =
  game {barPos = p', barSlit = s', barSpeed = v', rds = r', score = sc'}
  where
    (p, s, v, r, sc) =
      (barPos game, barSlit game, barSpeed game, rds game, score game)
    (p', s', v', r', sc') -- if p > -size_x_window then (p + speed_bar*sec , s, r) else (0,sNew,rNew)
      | p > -(sizeXWindow + sizeABars * 3) =
        (p + v * sec, s, v + speedDelta * sec, r, sc)
      | otherwise = (0, sNew, v + speedDelta * sec, rNew, sc + 1)
    sNew = head r
    rNew = tail r

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

bird :: Float -> Int -> Picture
bird y s = translate (negate (sizeXWindow / 4)) y (sprite s)
  where
    sprite 0 = birdAlive
    sprite 1 = rotate 15 birdAlive
    sprite 2 = rotate (-15) birdAlive
    sprite 3 = birdDead
    sprite 4 = rotate 90 birdDead
    sprite _ = blank

-- bird y = color color_bird $ translate (0-size_x_window/4) y $ circleSolid size_a_bird
bar :: Float -> Float -> Picture
bar x y = translate x (-y) barGen

bg0 :: Float -> Picture
bg0 x = translate x 0 bg0Pic

bg1 :: Float -> Picture
bg1 x = translate x 0 bg1Pic

debug :: GameState -> Picture
debug game = pictures [text $ show (barSpeed game)]

-- take Int relative position of the bottom of the slit:
-- i.e.: 0: slit bottom at top + 100
barGen :: Picture
barGen = pictures [top', bot']
  where
    top' = translate (sizeXWindow / 2 + 26) (sizeYWindow / 2 + 200) topBar
    bot' =
      translate
        (sizeXWindow / 2 + 26)
        (sizeYWindow / 2 - 400 - sizeASlit)
        botBar

scoreGen :: Int -> Picture
scoreGen int =
  translate (-225) 350 $ scale 3 3 $ pictures [translate (-8) 0 $ dig d, dig u]
  where
    dig i = bitmapSection (Rectangle (7 * i, 0) (7, 10)) numsSprite
    u = int `mod` 10
    d = int `div` 10

numsSprite :: BitmapData
numsSprite = pic2bmp nums

nums :: Picture
{-# NOINLINE nums #-}
nums = unsafePerformIO . loadBMP . getSprite $ "num"

pic2bmp :: Picture -> BitmapData
pic2bmp (Bitmap bmpData) = bmpData

displayList :: [Int] -> IO ()
displayList = putStrLn . helper
  where
    helper :: [Int] -> String
    helper []     = ""
    helper (n:ns) = show n ++ helper ns

displayList' :: [Int] -> IO ()
displayList' = foldr ((>>) . putStr . show) (putStrLn "")
