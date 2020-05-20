import Data.List
import Data.Char
--- HIGHER ORDER FUNCTIONS

--- 1. It Begins

--- 1.1

mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) = (f x):(mymap f xs)

--- List of sqrt from lsit of numbers

listSqrt :: (Floating a) => [a] -> [a]
listSqrt list = mymap sqrt list

--- List of lengths
listLength :: [String] -> [Int]
listLength list = mymap length list

--- toUpper
stringToUpper :: String -> String
stringToUpper string = mymap toUpper string

--- list of strings to upper
listToUpper :: [String] -> [String]
listToUpper [] = []
listToUpper (x:xs) = (stringToUpper x):(listToUpper xs)

--- add fav number
addFavNumber :: (Num a) => a -> [a] -> [a]
addFavNumber num list = mymap (+num) list

--- Double the numbers in a list unless threshold
doubleList xs = mymap step xs
  where step x | x > 5 = x
               | otherwise = 2*x
--- OR LAMBDA∷
--- mymap (\x -> if x > 5 then x*2 else x) [1,2,3,4,5,6]

--- Make a list of lists given a list
listOfList xs = mymap step xs
  where step x = [x]
--- OR LAMBDA∷
--- mymap (\x -> [x]) [1,2,3,4,5,6]


--- Fizzbuzz
fizzbuzz :: Int -> String
fizzbuzz x
  | x `mod` 3 == 0 && x `mod` 5 == 0 = "FizzBuzz"
  | x `mod` 3 == 0 = "Fizz"
  | x `mod` 5 == 0 = "Buzz"
  | otherwise = show x
---

--- Make a list of fizzbuzz of the first 20 numbers
--- mymap fizzbuzz [1..20]

--- Infinite list of squarenumbers
--- mymap (**2) [1..]

--- Zip list .....
--mymap (\(x,y) -> x+y) $ zip [1..] (mymap (**2) [1..])


--- 1.2 Filter
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter exp (x:xs)
  | exp x == True = x:(myfilter exp xs)
  | otherwise = myfilter exp xs

--- myfilter (<5) [1,2,3,4,5,6,7,8]
--- Own function that returns a bool
isFive :: (Num a, Eq a) => a -> Bool
isFive num = num == 5

--- quicksort ???????????????????????????????????????????????????????????????????????

--- factors
findFactors num = myfilter (\x -> num `mod` x == 0) [1..]

--- isPrime
isPrime 1 = False
isPrime num
  | head nums == 1 && head (tail nums) == num = True
  | otherwise = False
  where nums = findFactors num

--- first 10 primes
--- myfilter (\x -> isPrime x == True) [1..30]

--- 1.3 Fold

--- foldl examples ::
--- EXAMPLE: foldl mod 1337 [1166, 86, 43]
--- 1. Takes second argument (1337) and applies to first element in list
--- 2. Then uses the result of this (171) and applies to second element in list
--- 3. Keeps going until no more elements
--- Answer: foldl mod 1337 [1166, 86, 43] =
--- 1337 mod 1166 = 171
--- 171 mod 86 = 85
--- 85 mod 43 = 42
--- So answers i 42.

--- foldr does the same, but starting from end of list.
--- Make own fold functions ...
myfoldl f num [x] = f num x
myfoldl f num (x:xs) = myfoldl f (f num x) xs

myfoldr f num [x] = f x num -- foldr takes list ele first, then input var
myfoldr f num list = myfoldr f (f (last list) num) $ take (length list-1) list ++ []

-- Own functions using own fold
--- Own sum function
mySumFold :: (Num a) => [a] -> a
mySumFold list = myfoldl (+) 0 list

--- Own Factorial
myFactorial n = myfoldr (*) 1 [1..n]

--- own Max
myMaxFold list = myfoldl max 0 list

--- own reverse


myReverseFoldl list = myfoldl (\emp x -> x : emp) [] list
myReverseFoldr list = myfoldr (\x emp -> emp ++ [x]) [] list

-- own last -- not sure if this was how is should have been solved
lastl1 list = foldl1 (flip const) list

--- 2. Modules beginning


-- 2.1 Usage
chars = ['a'..'z']
pangram string = all (\x -> elem x (map toLower string)) chars

lengthFold list =  myfoldl (\x _ -> x+1) 0 list

elemFold list = myfoldl (\elem x -> x==elem) 5 list
