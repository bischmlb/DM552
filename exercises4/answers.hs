import Data.List
import Data.Char

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

moveAround :: Shape -> Shape
moveAround (Circle (Point x1 x2) r) = (Circle (Point x2 x1) r)
moveAround (Rectangle (Point x1 y1) (Point x2 y2)) = (Rectangle (Point y2 x1) (Point x2 y1))


--- 1.2
data Student = Student String String [String] [Int] deriving (Show)

addCourse :: Student -> String -> Student
addCourse (Student name examnum cour grades) course = (Student name examnum (cour ++ [course]) grades)

remCourse :: Student -> String -> Student
remCourse (Student name examnum [] grades) _ = (Student name examnum [] grades)
remCourse (Student name examnum (x:xs) grades) course
  | course == x = (Student name examnum (delete x (x:xs)) grades)
  | otherwise = remCourse (Student name examnum xs grades) course

addGrade :: Student -> Int -> Student
addGrade (Student name examnum courses grades) grade = (Student name examnum courses (grades ++ [grade]))

data Gender = Male | Female deriving (Show)
data Person = Person {
                      name :: String,
                      gender :: Gender,
                      age :: Int } deriving (Show)

data StudentNew = StudentNew Person String [String] [Int] deriving (Show)

greetingsPerson :: Person -> String
greetingsPerson (Person name _ _) = "Hello " ++ name ++ "!"

-- Record syntax: Look at how we defined Person (Record syntax)

checkRights :: Person -> Bool
checkRights (Person _ _ age) = age >= 18

votingRights :: [Person] -> [Person]
votingRights [] = []
votingRights (x:xs)
  | checkRights x = (persons ++ [x]) ++ votingRights xs
  | otherwise = persons ++ votingRights xs
  where persons = []

-- typechecking issue dunno
--checkGrades :: StudentNew -> Bool
--checkGrades (StudentNew _ _ _ grades) =  ((sum grades) / (length grades)) >= 4


-- 1.3 Trees

data Tree a = Nil | Node a Int (Tree a) (Tree a) deriving (Show)

singleton :: a -> Int -> Tree a
singleton x value = Node x value Nil Nil -- Makes it easier to create empty node

insertNode :: a -> Int -> Tree a -> Tree a
insertNode x value Nil = singleton x value -- necessary to find end of tree
insertNode x value (Node a key left right)
  | value < key = (Node a key (insertNode x value left) right) -- With typechecking the recursive call will find a leaf(end of tree), and we insert
  | value > key = (Node a key left (insertNode x value right))
  | otherwise = (Node x value left right)

keyExist :: Int -> Tree a -> Bool
keyExist value Nil = False
keyExist value (Node a key Nil Nil) = value == key
keyExist value (Node a key left right)
  | value < key = keyExist value left
  | value > key = keyExist value right
  | otherwise = True


--- 3.1 Charlike
class CharLike a where
  makeChar :: a -> Char
  isChar :: a -> Bool
  fromChar :: Char -> a
  isChar _ = True

instance CharLike Bool where
  makeChar True = 'T'
  makeChar False = 'F'
  fromChar 'T' = True
  fromChar 'F' = False

instance CharLike Char where
  makeChar c = c
  fromChar c = c
  isChar _ = True

instance CharLike Int where
  makeChar c = (intToDigit c)
  fromChar x = (digitToInt x)


--- 3.2 Game


--- 4 Functors
data MyMaybe a = MyJust a | MyNothing deriving (Show)
instance Functor MyMaybe where
  fmap f (MyJust x) = MyJust (f x)
  fmap _ MyNothing = MyNothing

 
