import Data.List


-- 1. Quickstart
---- Everything works ...


---------- 2. Exercises -----------

--2.1.1 Arithmetic--
add :: Int -> Int -> Int
add x y = x + y

sub :: Int -> Int -> Int
sub x y = x - y

mult :: Int -> Int -> Int
mult x y = x * y

divi :: Int -> Int -> Int
divi x y = x `div` y

--2.1.2 Boolean Logic--
-- True && True = True
-- False && True = False
-- False || True = True
-- not(False || False) = True
-- True == True = True
-- .....
-- pi == pi == 3.141592653589793 = True

--2.1.3 Dangeerous territory--
-- a lot of exceptions !!

--2.1.4 Simple functions
-- succ returns successor to int, so succ 3 = 4
-- min or max returns min or max of input x,y
-- will return something interesting

--2.1.5 Your own function--

-- Function that doubles a number:
doub :: Int -> Int
doub x = x*2

-- Function that adds doubles of two numbers:
doubAdd :: Int -> Int -> Int
doubAdd x y = doub x + doub y

-- Function that doubles only if it is smaller than 100
doubleSmaller :: Int -> Int
doubleSmaller x
  | x < 100 = doub x
  | otherwise = x


--2.1.6 Lists

-- 1 Creating and combining:
-- Create lists with numbers
numList = [1,2,3,4,5,6]
-- List with string
stringList = ["hej", "med", "dig"]
-- charList
charList = ['h', 'e', 'j']
-- Concatenate lists
newList1 = numList ++ numList
newList2 = stringList ++ stringList
newList3 = charList ++ charList
charListString = charList ++ "LOOOOOOOOOOOOOOOOL" -- It will convert the char list to string.
addToStart = 1:numList

-- 2 Indexing:
-- Syntax for getting fourth element (list!!index)
getFourth = newList2!!0 ++ newList2!!5
-- Result of "Hello World"!!8
-- Will give us index 8 of "Hello World" String in haskell should be seen as lists of chars, so it will give us 'r'
indexEight = "Hello World"!!8

-- 3 Small list functions
-- head = first element
headEx = head [1,2,3,4,5]
-- tail = every element that is not the head
tailEx = tail [1,2,3,4,5]
-- length = length
lengthEx = length [1,2,3,4,5]
-- null = check if list is null? or empty
nullEx = null []
-- reverse = return list reverse
reverseEx = reverse [1,2,3,4,5]
-- take = takes first couple of n elements here we take 1,2,3
take3Ex = take 3 [1,2,3,4,5]
-- maximum and minimum = self explanatory ..
maxEx = maximum [1,2,3,4,5]
minEx = minimum [1,2,3,4,5]
-- sum = self explanatory .. here = 15
sumEx = sum [1,2,3,4,5]
-- elem = checks if element n is in list
elem3Ex = 3 `elem` [1,2,3,4,5]

--4 Ranges and infinite lists
-- List of numbers from 23..42
rangeInt = [23..42]
-- Character range list A to P
rangeCharacters = ['A'..'P']
-- List every multiple of 4 and even numbers 42 to 20
multiple4 = [4,8..100]
evenNum4220 = [42,40..20]
-- Infinite list, display first few elements display first 20 of infinite list 1..??
infinite1 = take 20 [1,2..]
-- take first 20 of infinite list of 5's
repeatEx = take 20 (repeat 5)
-- cycle
cycleEx = take 20 (cycle[1,2,3])


-- 2.2 Better Functions

--2.2.1 First pattern matching
-- return 1 if x==1, 2 if x==2, x*2 if x != 1 || x != 2
differentBehav :: Int -> Int
differentBehav x
  | x == 1 = x
  | x == 2 = 5
  | otherwise = x*2

--2.2.2 More pattern matching
myhead :: [Int] -> Int
myhead (x:xs) = x

mytail :: [Int] -> [Int]
mytail (x:xs) = xs

checkIfEqual :: Int -> Int -> Bool
checkIfEqual x y
  | x == y = True
  | otherwise = False

mynull :: Int -> [Int] -> Bool
mynull y (x:xs)
  | checkIfEqual y x = True
  | checkIfEqual y x == False = False
  | otherwise = mynull y xs

-- mylast :: [Int] -> Int come back to these ..

-- 2.3 Lists, revisited (list comprehensions)
-- add favorite number to elements in list
addFav :: Int -> [Int] -> [Int]
addFav x y = [z + x | z <- y]
-- double number if doesnt exceed threshould
doubleNum :: Int -> [Int] -> [Int]
doubleNum x y = [z*2 | z <- y, z<=x]
-- let each element become its own list inside the list
makeEleList :: [Int] -> [[Int]]
makeEleList x = [[y] | y <- x]

-- Remove all vowels
isVowel :: Char -> Bool
isVowel x
  | x == 'a' = True
  | x == 'e' = True
  | x == 'i' = True
  | x == 'o' = True
  | x == 'u' = True
  | x == 'y' = True
  | otherwise = False

removeVowels :: String -> [Char]
removeVowels x = [z | z <- x, isVowel z == False]

getLengthStrList :: [String] -> [Int]
getLengthStrList x = [length z | z <- x]

getAtLeastFour :: [String] -> [String]
getAtLeastFour x = [z | z <- x, length z >= 4]

--2.4 Something old, something new (Tuples)
--2.4.1
