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


--- 3. Make the function
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
