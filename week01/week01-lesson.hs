hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1


hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)


intListLength :: [Integer] -> Integer
intListLength = foldr (\x -> (+) 1) 0
{-intListLength []     = 0-}
{-intListLength (x:xs) = 1 + intListLength xs-}


sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []
sumEveryTwo (x:[])     = [x]
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs
