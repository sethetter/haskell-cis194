{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}

module LectureWeek02
  ( mainLectureWeek02
  ) where

import CodeWorld

-- TODO: Code through lecture examples for practice!

type WorldState = (Direction, Coord)

data Tile
  = Wall
  | Ground
  | Storage
  | Box
  | Blank
  deriving (Eq)

data Direction = R | U | L | D

data Coord = C Integer Integer

wall, ground, storage, storageDot, box :: Picture
wall       = colored (gray 0.1) (solidRectangle 1 1)
ground     = colored (gray 0.5) (solidRectangle 1 1)
storageDot = colored orange (solidCircle 0.2)
storage    = storageDot & colored (gray 0.5) (solidRectangle 1 1)
box        = colored brown (solidRectangle 1 1)

player :: Picture
player =
  translated 0.1 0 $
    scaled 0.1 0.1 $
      translated 0 1.5 (solidCircle 0.7) & -- Head
      thickPath 0.2 [(0, 1), (0, -1)] & -- Body
      thickPath 0.2 [(0, 0), (1, 0)] & -- Arms
      thickPath 0.2 [(0, -1), (-0.5, -3)] & -- Left Leg
      thickPath 0.2 [(0, -1), (0.5, -3)] -- Right Leg

playerFacing :: Direction -> Picture
playerFacing R = rotated 0 player
playerFacing U = rotated (pi * 0.5) player
playerFacing L = rotated pi player
playerFacing D = rotated (pi * 1.5) player

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = blank

drawTileAt :: Integer -> Integer -> Picture
drawTileAt x y = translated (fromIntegral x) (fromIntegral y) $ drawTile $ maze $ C x y

draw21Times :: (Integer -> Picture) -> Picture
draw21Times something = go (-10)
  where
    go 11 = blank
    go n  = something n & go (n+1)

pictureOfMaze :: Picture
pictureOfMaze = draw21Times (draw21Times . drawTileAt)

maze1 :: Coord -> Tile
maze1 (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

maze2 :: Coord -> Tile
maze2 (C x y)
  | abs x > 4  || abs y > 4     = Blank
  | abs x == 4 || abs y == 4    = Wall
  | x > (-4) && x < 3 && y == 0 = Wall
  | y < 4 && y > 1 && x == 1    = Wall
  | y < 3 && y > 0 && x == -1   = Wall
  | x > -3 && x < 2 && y == -2  = Box
  | x == -3 && y < 4 && y > 0   = Storage
  | x == -2 && y == 1           = Storage
  | otherwise                   = Ground

maze :: Coord -> Tile
maze = maze2

initialState :: WorldState
initialState = (R, C 0 (-1))

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) = translated (fromIntegral x) (fromIntegral y)

adjacentCoord :: Direction -> Coord -> WorldState
adjacentCoord R (C x y ) = (R, C (x + 1)  y)
adjacentCoord U (C x y ) = (U, C  x      (y + 1))
adjacentCoord L (C x y ) = (L, C (x - 1)  y)
adjacentCoord D (C x y ) = (D, C  x      (y - 1))

drawState :: WorldState -> Picture
drawState (d, c) = atCoord c (playerFacing d) & pictureOfMaze

handleTime :: Double -> WorldState -> WorldState
handleTime _ s = s

handleEvent :: Event -> WorldState -> WorldState
handleEvent (KeyPress k) (prevDir, prevCoord)
  | k == "Right" = attemptMove (prevDir, prevCoord) (adjacentCoord R prevCoord)
  | k == "Up"    = attemptMove (prevDir, prevCoord) (adjacentCoord U prevCoord)
  | k == "Left"  = attemptMove (prevDir, prevCoord) (adjacentCoord L prevCoord)
  | k == "Down"  = attemptMove (prevDir, prevCoord) (adjacentCoord D prevCoord)
handleEvent _ c  = c

attemptMove :: WorldState -> WorldState -> WorldState
attemptMove prevState (newDir, newCoord)
  | maze newCoord == Ground  = (newDir, newCoord)
  | maze newCoord == Storage = (newDir, newCoord)
  | otherwise           = prevState

resetableInteractionOf ::
  WorldState ->
  (Double -> WorldState -> WorldState) ->
  (Event -> WorldState -> WorldState) ->
  (WorldState -> Picture) ->
  IO ()
resetableInteractionOf is ht he = interactionOf is ht (resetOnEsc he)
  where
    resetOnEsc :: (Event -> WorldState -> WorldState) -> Event -> WorldState -> WorldState
    resetOnEsc _ (KeyPress "Esc") _ = initialState
    resetOnEsc handler e prev = handler e prev

mainLectureWeek02 :: IO ()
mainLectureWeek02 = resetableInteractionOf initialState handleTime handleEvent drawState
