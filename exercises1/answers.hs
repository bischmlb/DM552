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


mylength :: [Int] -> Int
mylength [] = 0
mylength (x:xs) = 1 + mylength xs

mylast :: [Int] -> Int
mylast [] = error "Empty list.."
mylast [x] = x
mylast (x:xs) = mylast xs


mytake :: Int -> [Int] -> [Int]
mytake 0 _ = []
mytake x [] = []
mytake x (y:ys) = y : mytake (x-1) ys

myrepeat :: Int -> [Int]
myrepeat x = x : myrepeat x

myreplicate :: Int -> Int -> [Int]
myreplicate 0 _ = []
myreplicate x y = y : myreplicate (x-1) y

mymaximum :: [Int] -> Int
mymaximum [] = error "Empty List"
mymaximum [x] = x
mymaximum (x:xs)
  | x < (mymaximum xs) = mymaximum xs
  | otherwise = x

mysum :: [Int] -> Int
mysum [x] = x
mysum [] = 0
mysum (x:xs) = x + mysum xs

myreverse :: [Int] -> [Int]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

myelem :: Int -> [Int] -> Bool
myelem _ [] = False
myelem x (y:ys)
  | x == y = True
  | otherwise = myelem x ys


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

-- Used to check if String has vowels in removeVowels
isVowel :: Char -> Bool
isVowel x
  | x == 'a' = True
  | x == 'e' = True
  | x == 'i' = True
  | x == 'o' = True
  | x == 'u' = True
  | x == 'y' = True
  | otherwise = False

-- Remove all vowels
removeVowels :: String -> [Char]
removeVowels x = [z | z <- x, isVowel z == False]

getLengthStrList :: [String] -> [Int]
getLengthStrList x = [length z | z <- x]

getAtLeastFour :: [String] -> [String]
getAtLeastFour x = [z | z <- x, length z >= 4]

--2.4 Something old, something new (Tuples)
--2.4.1 First Tuples

nameList = ["Mathias", "Marcus", "Hans", "Bo", "Lars", "Jakob"]
ageList = [22,21,17,34,53,12]
-- Use zip to make list of persons (name, age)
zipEx = zip nameList ageList
-- List comprehensions to filter persons - people starting with 'A' or people with more than 5 letters
filterPersons1 = [x | x <- zipEx, head (fst x) == 'M']
filterPersons2 = [x | x <- zipEx, snd x >= 18]
filterPersons3 = [x | x <- zipEx, length (fst x) > 5]
-- Function with different patterns and behavior
patternFunc :: (String, Int) -> String
patternFunc (x, y)
  | head x == 'A' = "You're in the A-club"
  | length x > 5 = "That's one long ass name"
  | y > 20 = "You're old as shit"
  | otherwise = "You suck."

-- Triple tuple birthday exercise
-- If current month is lower than birth month, we know that it has not been the persons birthday yet
-- If current month is higher, we know the birthday has been surpassed
-- If birthmonth and current month is higher, we compare dates:
-- If date is higher .. surpassed, if not .. we have not had birthday yet.


-- returnAge :: (birthday d-m-y) -> (current date d-m-y) -> age
returnAge :: (Int, Int, Int) -> (Int, Int, Int) -> Int
returnAge (x,y,z) (xx,yy,zz)
  | yy < y = (zz - z) - 1
  | yy > y = zz - z
  | yy == y && xx < x = (zz - z) -1
  | yy == y && xx >= x = zz - z
  | otherwise = 0

--2.4.2 More tuples
-- 1∷
calcDistanceOrigin :: (Double, Double) -> Double
calcDistanceOrigin (x, y) = sqrt((x-0)**2 + (y-0)**2)

--2∷
listX = [3.0, 5.3, 7.1, 4.3, 9.5, 6.0, 7.0]
listY = [5.3, 6.1, 2.9, 9.0, 8.2, 9.3, 1.4]
xyCoords = zip listX listY

--3∷
numss = [5,3,5,6,8,1,4,5,0,2,4,5,1,2,3]
points = [(x,y) | x <- numss, y <- [1..10]]

--4∷
listOfDist = [calcDistanceOrigin x | x <- xyCoords]

--5∷
calcDistance :: (Double, Double) -> (Double, Double) -> Double
calcDistance (x, y) (xx, yy) = sqrt((x-xx)**2 + (y-yy)**2)

--6∷ Assumed a pair is listed in 4-tuple as (x1,y1,x2,y2)
pairsPoints = [(x,y,xx,yy) | x <- [1..5], y <- [1..5], xx <- [1..5], yy <-[1..5]]

--7∷
calcPairPoints :: (Double, Double, Double, Double) -> Double
calcPairPoints (x,y,xx,yy) = calcDistance (x,y) (xx,yy)
listPairsPoints = [calcPairPoints x | x <- pairsPoints]
