module HW02 where
import qualified Data.List as L
-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches _ [] = 0
exactMatches [] _ = 0
exactMatches (x:xs) (y:ys)
  | x == y = 1 + exactMatches xs ys
  | otherwise = exactMatches xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = foldr recordOne [] colors
  where
    recordOne :: Peg -> [Int] -> [Int]
    recordOne x acc = (countOne x code):acc
    countOne :: Peg -> Code -> Int
    countOne _ [] = 0
    countOne p (x:xs)
          | p == x = 1 + countOne p xs
          | otherwise = countOne p xs


-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches x y = sum $ minInt (countColors x) (countColors y)
  where
    minInt :: [Int] -> [Int] -> [Int]
    minInt [] _ = []
    minInt _ [] = []
    minInt (p:ps) (q:qs)
      | p <= q = p : minInt ps qs
      | otherwise = q : minInt ps qs

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exactMatch (totalMatch - exactMatch)
  where exactMatch = (exactMatches secret guess)
        totalMatch = (matches secret guess)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move@(Move guess _ _) code =
  compareMove move (getMove code guess)
  where compareMove (Move _ e1 t1) (Move _ e2 t2)
          | e1 == t1 && e2 == t2 = True
          | otherwise = False

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = L.filter (\x -> isConsistent move x) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = [[x] | x <- colors]
allCodes m = [x : y | x <- colors, y <- allCodes (m - 1)]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = [getMove secret x | x <- (allCodes 4)]

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
