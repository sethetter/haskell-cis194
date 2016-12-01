-- Enumeration types

data Thing
  = Shoe
  | Ship
  | SealingWax
  | Cabbage
  | King
  deriving (Show)

isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _    = True

listOfThings :: [Thing]
listOfThings = [Shoe, Ship, SealingWax, Cabbage, King]

-- Algebraic data types

data FailableDouble
  = Failure
  | OK Double
  deriving (Show)

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK x)  = x

data Person
  = Person String Integer Thing
  deriving (Show)

brent :: Person
brent = Person "Brent" 42 Cabbage

seth :: Person
seth = Person "Seth" 26 Shoe

getAge :: Person -> Integer
getAge (Person _ age _) = age

getName :: Person -> String
getName (Person name _ _) = name

personInfo :: Person -> String
personInfo p@(Person name _ _) = "The name for (" ++ show p ++ ") is " ++ name

checkFav :: Person -> String
checkFav (Person name _ Shoe) = name ++ "! You're my favorite!"
checkFav (Person name _ _)    = "We probably won't get along, " ++ name ++ ", sorry."
