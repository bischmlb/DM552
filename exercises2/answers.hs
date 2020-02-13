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

swap :: (a, a) -> (a, a)
swap (x, y) = (y, x)

pair :: a -> a -> (a, a)
pair x y = (x, y)

palindrome :: (Eq a) => [a] -> Bool
palindrome list = reverse list == list

twice :: f a -> a
twice swap x = swap x
