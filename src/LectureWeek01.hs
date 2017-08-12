module LectureWeek01 
  ( mainLectureWeek01
  ) where

import CodeWorld


botCircle c = colored c (translated 0 (-1.5) (solidCircle 1))
topCircle c = colored c (translated 0   1.5  (solidCircle 1))
frame = rectangle 2.5 5.5


trafficLight :: Bool -> Picture
trafficLight True  = botCircle green & topCircle black & frame
trafficLight False = botCircle black & topCircle red   & frame


trafficController :: Double -> Picture
trafficController t
  | round (t/3) `mod` 2 == 0 = trafficLight True
  | otherwise                = trafficLight False


spread :: Picture -> Double -> Integer -> Picture
spread pic dx 0 = blank
spread pic dx n = pic & translated dx 0 (spread pic dx (n-1))


growingPath :: Double -> Picture
growingPath t = path [(0,0), (0, sin t * 5)]


circleOfLines :: Double -> Double -> Picture
circleOfLines n t = nextLineForCircle t n n


nextLineForCircle :: Double -> Double -> Double -> Picture
nextLineForCircle t n 0 = blank
nextLineForCircle t n i = rotated ((2 * pi/n) * i) (growingPath t) & nextLineForCircle t n (i-1)


ourPicture :: Picture
ourPicture = circleOfLines 40 1


ourAnimation :: Double -> Picture
ourAnimation = circleOfLines 40



mainLectureWeek01 :: IO ()
mainLectureWeek01 = animationOf ourAnimation
{- mainLectureWeek01 = drawingOf ourPicture -}
