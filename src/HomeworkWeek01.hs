{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}

module HomeworkWeek01
  ( mainHomeworkWeek01
  ) where


import CodeWorld


mainHomeworkWeek01 :: IO ()
mainHomeworkWeek01 = exercise3

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

data LightState = Go | Stop | Slow | Ready

botCircle, midCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-3) (solidCircle 1))
midCircle c = colored c (translated 0   0  (solidCircle 1))
topCircle c = colored c (translated 0   3  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 9

trafficLight :: LightState -> Picture
trafficLight Go    = botCircle green & midCircle black  & topCircle black & frame
trafficLight Stop  = botCircle black & midCircle black  & topCircle red   & frame
trafficLight Slow  = botCircle black & midCircle yellow & topCircle black & frame
trafficLight Ready = botCircle black & midCircle yellow & topCircle red   & frame

nextState :: LightState -> LightState
nextState Go    = Slow
nextState Slow  = Stop
nextState Stop  = Ready
nextState Ready = Go

trafficController :: Int -> Picture
trafficController s
  | s >= 0 && s < 3 = trafficLight Go
  | s == 3          = trafficLight Slow
  | s >= 4 && s < 7 = trafficLight Stop
  | otherwise       = trafficLight Ready

trafficLightAnimation :: Double -> Picture
trafficLightAnimation t = trafficController (round t `mod` 8)

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

tree :: Picture -> Integer -> Picture
tree _ 0 = blank
tree b n = path [(0,0),(0,1)] & translated 0 1 b & translated 0 1 (
  rotated (pi/10) (tree b (n-1)) & rotated (- pi/10) (tree b (n-1)))

blossom :: Double -> Picture
blossom t
  | t <= 10   = colored yellow (solidCircle (t / 50))
  | otherwise = colored yellow (solidCircle 0.2)

bloomingTree :: Double -> Picture
bloomingTree t = tree (blossom t) 8

exercise2 :: IO ()
exercise2 = animationOf bloomingTree

-- Exercise 3

wall, ground, storage, box :: Picture
wall       = colored (gray 0.3) (solidRectangle 1 1)
ground     = colored black (solidRectangle 1 1)
storageDot = colored orange (solidCircle 0.2)
storage    = storageDot & colored black (solidRectangle 1 1)
box        = colored brown (solidRectangle 1 1)

drawTile :: Integer -> Picture
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = storage
drawTile 4 = box
drawTile _ = blank

drawRows :: Integer -> Picture
drawRows 11 = blank
drawRows y  = drawColumns y (-10) & drawRows (y + 1)

drawColumns :: Integer -> Integer -> Picture
drawColumns _ 11 = blank
drawColumns y x  = drawTileAt x y & drawColumns y (x + 1)

drawTileAt :: Integer -> Integer -> Picture
drawTileAt x y = translated (fromIntegral x) (fromIntegral y) (drawTile (maze x y))

pictureOfMaze :: Picture
pictureOfMaze = drawRows (-10)

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
