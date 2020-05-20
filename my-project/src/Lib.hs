module Lib (
 generateRandom,
 isValid,
 hasWinningStrategy
 ) where

import System.IO
import Control.Monad
import System.Random
import Data.Array.IO


data Player = Blue | Red deriving (Eq, Show)
data Status = Running | GameOver deriving (Eq, Show)


type Move = ((Int,Int), (Int, Int), [Char])
type PieceList = [(Int, Int)]
type Coordinate = (Int, Int)
type Card = String

---- pattern matching for tuple type
fst' (a,_,_) = a
snd' (_,b,_) = b
thd' (_,_,c) = c
------------------------------------

data CardMoves = CardMoves { cobra :: [Coordinate]
                            , rabbit :: [Coordinate]
                            , rooster :: [Coordinate]
                            , tiger :: [Coordinate]
                            , monkey :: [Coordinate]
                            } deriving (Eq, Show)

data Game = Game { gameCards :: [Card]
                  , gameTurn :: Player
                  , gamePiecesBlue :: PieceList
                  , gamePiecesRed :: PieceList
                  , recentMove :: Move
                  , gameState :: Status
                  } deriving (Eq, Show)



possibleCards = ["Cobra", "Rabbit", "Rooster", "Tiger", "Monkey"]


possibleMoves = CardMoves { cobra = [(-1,0), (1,1), (1,-1)]
                            , rabbit = [(-1,-1), (1,1), (2,0)]
                            , rooster = [(-1,-1), (-1,0), (1,0), (1,1)]
                            , tiger = [(0,2), (0,-1)]
                            , monkey = [(-1,1), (1,1), (-1,-1), (1,-1)]}


switchPlayer :: Game -> Game
switchPlayer game
  | (gameTurn game) == Blue = game {gameTurn = Red}
  | (gameTurn game) == Red = game {gameTurn = Blue}


checkStatus :: Game -> IO (String)
checkStatus game = do
  let status = (gameState game)
  if status == GameOver
    then return "Game Over"
    else return "Game Running"

checkTurn :: Game -> Player
checkTurn (Game _ gt _ _ _ _) = gt

makeMove :: Game -> Int -> Game -- Parameters: Game, card index, move (to make), piece to move
makeMove game seed = do -- card = index of players 2 cards, (card 0 or 1)
  let turn = checkTurn game
  let gamePieces = if (turn == Blue)
                  then (gamePiecesBlue game)
                  else (gamePiecesRed game)
  let playerCards = if (turn == Blue)
                  then [] ++ [(gameCards game) !! 0] ++ [(gameCards game) !! 1]
                  else [] ++ [(gameCards game) !! 2] ++ [(gameCards game) !! 3]
  let card = chooseRandomCard (mkStdGen seed)
  let possibleTurns = turnsPossible card playerCards

  let move = possibleTurns !! (chooseRandomMove (length possibleTurns) (mkStdGen seed))
  let piece = gamePieces !! (chooseRandomPiece (length gamePieces ) (mkStdGen seed))
  let newGamePieces = removeItem piece gamePieces
  let newPiece = ((fst piece)+(fst move), (snd piece)+(snd move))

  let fullMove = (piece, newPiece, (playerCards !! card))
  --- update game variables
  let initialWPieces = updatePieces game (chooseRandomPiece (length gamePieces ) (mkStdGen seed)) newGamePieces newPiece
  let initialWCards = updateCards initialWPieces card playerCards
  let initialWTurn = switchPlayer initialWCards
  let newGame = updateMove initialWTurn fullMove
  newGame

-- when use player card, need tail of total array append to player, then construct both arrays
updateCards :: Game -> Int -> [Card] -> Game
updateCards gm cd cards = do
  let cardName = (cards !! cd)
  let arr = removeItem cardName cards
  let newPlayerCards = arr ++ [(last(gameCards gm))]
  let opponentCards = if ((gameTurn gm) == Blue)
                   then [] ++ [(gameCards gm) !! 2] ++ [(gameCards gm) !! 3]
                   else [] ++ [(gameCards gm) !! 0] ++ [(gameCards gm) !! 1]
  let newArray = if ((gameTurn gm) == Blue)
                then newPlayerCards ++ opponentCards ++ [cardName]
                else opponentCards ++ newPlayerCards ++ [cardName]

  gm { gameCards = newArray }


turnsPossible :: Int -> [String] -> [(Int, Int)]
turnsPossible n arr
  | (arr !! n) == "Cobra" = (cobra possibleMoves)
  | (arr !! n) == "Rabbit" = (rabbit possibleMoves)
  | (arr !! n) == "Rooster" = (rooster possibleMoves)
  | (arr !! n) == "Tiger" = (tiger possibleMoves)
  | otherwise = (monkey possibleMoves)

updatePieces :: Game -> Int -> [(Int,Int)] -> (Int, Int) -> Game
updatePieces gm ind arr newPiece -- ind = 0 means sensei piece, 1 means pawn
  | ind == 0 && (gameTurn gm == Blue) =
    gm { gamePiecesBlue = newPiece:arr }
  | ind == 0 && (gameTurn gm == Red) =
    gm { gamePiecesRed = newPiece:arr }
  | ind /= 0 && (gameTurn gm == Blue) =
    gm { gamePiecesBlue = arr ++ [newPiece] }
  | ind /= 0 && (gameTurn gm == Red) =
    gm { gamePiecesRed = arr ++ [newPiece] }

updateMove :: Game -> Move -> Game
updateMove gm mv = gm { recentMove = mv }

chooseRandomMove :: Int -> StdGen -> Int
chooseRandomMove n gen =
  let (int, newGen) = randomR(0, n-1) gen
  in int

chooseRandomPiece :: Int -> StdGen -> Int
chooseRandomPiece n gen =
  let (int, newGen) = randomR(0, n-1) gen
  in int

chooseRandomCard :: StdGen -> Int
chooseRandomCard gen =
  let (int, newGen) = randomR(0, 1) gen
  in int


--- TODO: Make it possible for makemove to make random moves
--- TODO: Change and store game variables accordingly
--- TODO: Make loop for actually generating moves???
--- GENERATE THE N MOVES IN A LOOP AND APPEND TO IO STRING ...

--- MAKE INITIAL TABLE ----
randomInitial :: StdGen -> [String]
randomInitial gen =
  let (int1, newGen) = randomR(0,4) gen
      (int2, newGen') = randomR(0,3) newGen
      (int3, newGen'') = randomR(0,2) newGen'
      (int4, newGen''') = randomR(0,1) newGen''
      string1 = (possibleCards !! int1)
      newList = removeItem string1 possibleCards
      string2 = (newList !! int2)
      newList2 = removeItem string2 newList
      string3 = (newList2 !! int3)
      newList3 = removeItem string3 newList2
      string4 = (newList3 !! int4)
      newList4 = removeItem string4 newList3
      string5 = (newList4 !! 0) -- size will be 1 and we will get last element available
  in  [string1, string2, string3, string4, string5]

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

--- PRINTERS ---
printTable :: Game -> String
printTable table = do
  let game = (gameCards table)
  let piecesP1 = (gamePiecesBlue table)
  let piecesP2 =  (gamePiecesRed table)
  "(" ++ show game ++ show piecesP1 ++ show piecesP2 ++ ")" ++ "\n"


printMove :: Game -> String
printMove table = do
  let move = (recentMove table)
  show move ++ "\n"
-------------------------------------------------------
--TESTER
initt = Game { gameCards = randomInitial (mkStdGen 100)
                    , gameTurn = Blue
                    , gamePiecesBlue = [(0,2),(0,0),(0,1),(0,3),(0,4)]
                    , gamePiecesRed = [(4,2),(4,0),(4,1),(4,3),(4,4)]
                    , recentMove = ((0,0),(0,0),"null")
                    , gameState = Running }

--- FUNCTIONALITY ---

generateRandom :: Int -> Int -> IO (String)
generateRandom x y = do
  let game = Game { gameCards = randomInitial (mkStdGen x)
                      , gameTurn = Blue
                      , gamePiecesBlue = [(0,2),(0,0),(0,1),(0,3),(0,4)]
                      , gamePiecesRed = [(4,2),(4,0),(4,1),(4,3),(4,4)]
                      , recentMove = ((0,0),(0,0),"null")
                      , gameState = Running }

  let initialTable = printTable game
  let game1 = makeMove game x
  let p1 = printMove game1
  let total = gameLoop x y game1 "" -- append to empty string
  return (initialTable ++ total)

--- GAMELOOP FOR GENERATING RANDOM GAMES ---
gameLoop :: Int -> Int -> Game -> String -> String
gameLoop _ 0 _ s = s -- return final string when n==0
gameLoop seed n gm string  | (gameState gm) == GameOver = "Game Over."
                           | otherwise = do

    let move = printMove gm
    let newString = string ++ move
    let newGame = makeMove gm seed
    gameLoop seed (n-1) newGame newString
----------------------------------------------------------

isValid :: FilePath -> IO (String)
isValid _ = return "Not yet implemented"

hasWinningStrategy :: Int -> FilePath -> IO (String)
hasWinningStrategy _ _ = return "Not yet implemented"
