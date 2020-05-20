import Data.List
import Data.Char

main = do
  putStrLn "Hello, whats your name?"
  getname <- getLine
  putStrLn ("Hello " ++ getname ++ "!")
