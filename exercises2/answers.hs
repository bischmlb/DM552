import Data.List

--- 1. What is the type?

-- [Char]
-- (Char, Char, Char) (tuple)
-- (Bool, Char)
-- [(Bool, Char)]
-- ([Bool], [Char])
-- [a] -> [a]
-- [a] -> [a]
-- [[a],[a]](not correct) [[a] -> [a]]

--- 2. What was the type?
-- length :: [a] -> Int
-- head :: [a] -> a
-- null :: [a] -> Bool
-- take :: Int -> [a] -> [a]
-- maximum :: [a] -> a
-- sum :: [Num] -> Num
-- elem :: a -> [a] -> Bool
-- repeat :: a -> [a]
-- cycle :: [a] -> [a]
-- succ :: a -> a

--- 3. Let and where
roots :: (Floating t) => t -> t -> t -> (t,t)
roots a b c = (x,y)
  where x = e + sqrt d / (2 * a)
        y = e - sqrt d / (2 * a)
        d = b * b - 4 * a * c
        e = - b / (2 * a)

roots2 :: (Floating t) => t -> t -> t -> (t,t)
roots2 a b c =
  let x = e + sqrt d / (2 * a)
      y = e - sqrt d / (2 * a)
      d = b * b - 4 * a * c
      e = - b / (2 * a)
  in (x,y)

--- 4. Make the function
second :: [a] -> a
second (x:y:xs) = y

secondLast :: [a] -> a
secondLast list = list!!(length list - 2) -- (-2) because zero indexed

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

palindrome :: (Eq a) => [a] -> Bool
palindrome list = reverse list == list

twice :: (a -> a) -> a -> a
twice f x = f (f x)

flatten :: [[a]] -> [a]
flatten [] = []
flatten [x] = x
flatten (x:xs) = head [x] ++ flatten xs

alternate :: (Num a) => [a] -> [a]
alternate [] = []
alternate (x:y:xs) = [x] ++ [y*(-1)] ++ alternate xs

setIdx :: [a] -> a -> Int -> [a]
setIdx list ele index = ((take index list) ++ [ele]) ++ drop (index + 1) list -- use take to take first index element from list, append element, then drop first index+1 elements and append

--Used to test modIdx
multiplyBy3 :: (Num a) => a -> a
multiplyBy3 x = x*3

modIdx :: (a -> a) -> Int -> [a] -> [a]
modIdx f index list = ((take index list)) ++ [f (list!!index)] ++ drop (index + 1) list

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
  | x `elem` newlist = newlist ++ (unique xs)
  | otherwise = ([x] ++ newlist) ++ unique xs
  where newlist = []


--- 5. What is the type? (2)
-- ( + ) 42 = Num a => a -> a
-- ( : ) 3 = Num a => [a] -> [a]
-- ( , ) 'x' = b -> (Char, b)
-- ( , , ) "hi" = b -> c -> ([Char], b, c)

-- 6. Suggest the type
one :: a -> Int
one x = 1

--apply :: (a -> a) -> a -> (a -> a)
--compose :: (a -> a) -> (a -> a) -> a -> ((a -> a) -> ((a -> a)))


--- 7. Show me what you're hiding
plustalk :: Int -> Int -> String
plustalk x y = show x ++ " + " ++ show y ++ " is " ++ show (x+y)

fizzbuzz :: Int -> String
fizzbuzz x
  | x `mod` 3 == 0 && x `mod` 5 == 0 = "FizzBuzz"
  | x `mod` 3 == 0 = "Fizz"
  | x `mod` 5 == 0 = "Buzz"
  | otherwise = show x
