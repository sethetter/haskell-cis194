-- validate a cc number
-- double every other digit beginning on the right
-- add together all the digits (16 = 1 + 6)
-- valid number == prev number divisible by 10


toDigits :: Integer -> [Integer]
toDigits n
  | n < 0     = []
  | otherwise = fmap (\x -> read [x] :: Integer) (show n)


toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []  = []
doubleEveryOther [x] = [x]
doubleEveryOther lst = doubleEveryOther (init (init lst)) ++ (2 * last (init lst) : [last lst])


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits lst
  | head lst < 9 = head lst + sumDigits (tail lst)
  | otherwise    = sumDigits (toDigits (head lst)) + sumDigits (tail lst)


validate :: Integer -> Bool
validate cc = (sumDigits (doubleEveryOther (toDigits cc)) `rem` 10) == 0


-- Towers of Hanoi!
-- 1. move n - 1 discs from a to c using b as temporary storage
-- 2. move the top disc from a to b
-- 3. move n -1 discs from c to b using a as temporary storage


type Peg = String
type Move = (Peg, Peg)


-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _          = []
hanoi 1 start end _    = [(start, end)]
hanoi n start end temp =
  let nMinusOne = subtract 1 n
  in hanoi nMinusOne start temp end ++ hanoi 1 start end temp ++ hanoi nMinusOne temp end start
