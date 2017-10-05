{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}

module HomeworkWeek03
  ( mainHomeworkWeek03
  ) where

import CodeWorld

data List a = Empty | Entry a (List a)

data State = State PlayerState BoxStates

type PlayerState = (Direction, Coord)

type BoxStates = List Coord

data Tile
  = Wall
  | Ground
  | Storage
  | Box
  | Blank
  deriving (Eq)

data Direction = R | U | L | D

data Coord = C Integer Integer

data SSState world = StartScreen | Running world

data Interaction world = Interaction
  world
  (Double -> world -> world)
  (Event -> world -> world)
  (world -> Picture)

-- Drawings

startScreen :: Picture
startScreen = scaled 3 3 (text "SOKOBAN")

winScreen :: Picture
winScreen = scaled 3 3 (text "YOU WON!")

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

-- Draw Functions

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = blank

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) = translated (fromIntegral x) (fromIntegral y)

drawMazeTileAt :: Integer -> Integer -> Picture
drawMazeTileAt x y = atCoord (C x y) $ drawTile $ noBoxMaze $ C x y

draw21Times :: (Integer -> Picture) -> Picture
draw21Times something = go (-10)
  where
    go 11 = blank
    go n  = something n & go (n+1)

pictureOfMaze :: Picture
pictureOfMaze = draw21Times (draw21Times . drawMazeTileAt)

pictureOfBoxes :: BoxStates -> Picture
pictureOfBoxes Empty = blank
pictureOfBoxes (Entry c cs) = atCoord c (drawTile Box) & pictureOfBoxes cs

initialState :: State
initialState = State initialPlayerState initialBoxStates

initialPlayerState :: PlayerState
initialPlayerState = (R, C 0 (-1))

initialBoxStates :: BoxStates
initialBoxStates = go (-10) (-10)
  where
    go 11 11 = Empty
    go x  11 = go (x+1) (-10)
    go x  y  = case maze (C x y) of
      Box -> Entry (C x y) (go x (y+1))
      _   ->                go x (y+1)

drawState :: State -> Picture
drawState s@(State (d, c) boxes) =
  if isWon s then winScreen
             else atCoord c (playerFacing d) & pictureOfBoxes boxes & pictureOfMaze

-- Mazes!

maze :: Coord -> Tile
maze = maze1

noBoxMaze :: Coord -> Tile
noBoxMaze c =
  case maze c of
    Box -> Ground
    t   -> t

mazeWithBoxes :: BoxStates -> Coord -> Tile
mazeWithBoxes boxes c =
  if containsCoord c boxes then Box else noBoxMaze c

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

isOnStorage :: Coord -> Bool
isOnStorage c = noBoxMaze c == Storage

isGroundOrStorage :: BoxStates -> Coord -> Bool
isGroundOrStorage boxes coord =
  case mazeWithBoxes boxes coord of
    Ground  -> True
    Storage -> True
    _       -> False

-- Handlers

handleTime :: Double -> State -> State
handleTime _ s = s

handleEvent :: Event -> State -> State
handleEvent e state@(State (_, playerCoord) boxes) =
  if isWon state then state
  else case directionFromEvent e of
    Just direction ->
      if moveSucceeds direction state then
        State (direction, adjacentCoord direction playerCoord) (updateBoxes direction playerCoord boxes)
      else
        State (direction, playerCoord) boxes
    Nothing -> state

moveSucceeds :: Direction -> State -> Bool
moveSucceeds direction (State (_, playerCoord) boxStates) =
  if adjacentIsBox then isGroundOrStorage boxStates $ (moveInDir . moveInDir) playerCoord
                   else isGroundOrStorage boxStates $ moveInDir playerCoord
  where
    moveInDir = adjacentCoord direction
    adjacentIsBox = containsCoord (moveInDir playerCoord) boxStates

updateBoxes :: Direction -> Coord -> BoxStates -> BoxStates
updateBoxes _ _ Empty = Empty
updateBoxes direction playerCoord (Entry c cs) =
  if coordEq newPlayerCoord c then Entry (adjacentCoord direction c) (updateBoxes direction playerCoord cs)
                              else Entry c (updateBoxes direction playerCoord cs)
  where newPlayerCoord = adjacentCoord direction playerCoord

isWon :: State -> Bool
isWon (State _ boxes) = allList $ mapList isOnStorage boxes

-- Utility

directionFromEvent :: Event -> Maybe Direction
directionFromEvent (KeyPress k)
  | k == "Right" = Just R
  | k == "Up"    = Just U
  | k == "Left"  = Just L
  | k == "Down"  = Just D
  | otherwise    = Nothing
directionFromEvent _ = Nothing

containsCoord :: Coord -> List Coord -> Bool
containsCoord _  Empty       = False
containsCoord c (Entry x xs) = coordEq c x || containsCoord c xs

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y ) = C (x + 1)  y
adjacentCoord U (C x y ) = C  x      (y + 1)
adjacentCoord L (C x y ) = C (x - 1)  y
adjacentCoord D (C x y ) = C  x      (y - 1)

coordEq :: Coord -> Coord -> Bool
coordEq (C x1 y1) (C x2 y2) = x1 == x2 && y1 == y2

allList :: List Bool -> Bool
allList (Entry True rest) = allList rest
allList (Entry False _  ) = False
allList  Empty            = True

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry a as) = Entry (f a) (mapList f as)

-- Interactions

resetable :: Interaction s -> Interaction s
resetable (Interaction startState tick handle draw) =
  Interaction startState tick handle' draw
  where
    handle' (KeyPress "Esc") _ = startState
    handle' e                s = handle e s

withStartScreen :: Interaction s -> Interaction (SSState s)
withStartScreen (Interaction startState tick handle draw) =
  Interaction startState' tick' handle' draw'
  where
    startState' = StartScreen

    tick' _ StartScreen = StartScreen
    tick' t (Running s) = Running (tick t s)

    handle' (KeyPress " ") StartScreen = Running startState
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s

runInteraction :: Interaction s -> IO ()
runInteraction (Interaction startState tick handle draw) =
  interactionOf startState tick handle draw

-- MAIN!

mainHomeworkWeek03 :: IO ()
mainHomeworkWeek03 =
  runInteraction $
    (resetable . withStartScreen)
    (Interaction initialState handleTime handleEvent drawState)
