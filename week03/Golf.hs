module Golf where

skips :: [a] -> [[a]]
skips lst = foldr (everyNthOfList lst) [] [1..length lst]
  where everyNthOfList lst x acc = map fst (filter (\y -> snd y `mod` x == 0) (zip lst [1..])) : acc


