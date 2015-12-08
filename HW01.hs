module HW01 where

lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = (x - lastDigit x) `div` 10

toRevDigits :: Integer -> [Integer]
toRevDigits x
  | x < 0 = []
  | x < 10 = [x]
  | otherwise = (lastDigit x) : (toRevDigits $ dropLastDigit x)

toDigits :: Integer -> [Integer]
toDigits = reverse . toRevDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:[]) = x:[y * 2]
doubleEveryOther (x:y:xs) = x:(y * 2):(doubleEveryOther xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x]
  | x < 10 = x
  | otherwise = sumDigits . toDigits $ x
sumDigits (x:xs)
  | x < 10 = x + (sumDigits xs)
  | otherwise = (sumDigits . toDigits $ x) + (sumDigits xs)

luhn :: Integer -> Bool
luhn x = 0 == ((sumDigits . doubleEveryOther . toRevDigits $ x) `mod` 10)
