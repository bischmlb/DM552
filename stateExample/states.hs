
import Control.Monad.State

type Stack = [Int]

initialStack :: Stack
initialStack = [3,1,2,5,1]

popp :: State Stack Int
popp = state $ \(x:xs) -> (x,xs)

pushh :: Int -> State Stack ()
pushh a = state $ \xs -> ((), a:xs)

stackManipp :: State Stack Int
stackManipp = do
  pushh 3
  a <- popp
  popp

stackyStack :: State Stack ()
stackyStack = do
  currentStack <- get
  if currentStack == [1,2,3]
    then put [8,3,1]
    else put [9,2,1]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((),a:xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((), newStack1) = push 3 stack
    (a, newStack2) = pop newStack1
    in pop newStack2
