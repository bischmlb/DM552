module Lib (
 generateRandom,
 isValid,
 hasWinningStrategy
 ) where

import System.IO
import Control.Monad
import System.Random
import Data.List


data Player = Blue | Red | None deriving (Eq, Show)
data Status = Running | GameOver deriving (Eq, Show)


type Move = ((Int,Int), (Int, Int), [Char])
type PieceList = [Coordinate]
type Coordinate = (Int, Int)
type Card = String

---- pattern matching for move type
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
                  , winner :: Player
                  } deriving (Eq, Show)



possibleCards = ["Cobra", "Rabbit", "Rooster", "Tiger", "Monkey"]


possibleMovesBlue = CardMoves { cobra = [(-1,0), (1,1), (1,-1)]
                            , rabbit = [(-1,-1), (1,1), (2,0)]
                            , rooster = [(-1,-1), (-1,0), (1,0), (1,1)]
                            , tiger = [(0,2), (0,-1)]
                            , monkey = [(-1,1), (1,1), (-1,-1), (1,-1)]}

possibleMovesRed = CardMoves { cobra = [(1,0), (-1,-1), (-1,1)]
                            , rabbit = [(1,1), (-1,-1), (-2,0)]
                            , rooster = [(1,1), (1,0), (-1,0), (-1,-1)]
                            , tiger = [(0,-2), (0,1)]
                            , monkey = [(1,-1), (-1,-1), (1,1), (-1,1)]}


switchPlayer :: Game -> Game
switchPlayer game
  | (gameTurn game) == Blue = game {gameTurn = Red}
  | (gameTurn game) == Red = game {gameTurn = Blue}



checkTurn :: Game -> Player
checkTurn (Game _ gt _ _ _ _ _) = gt

makeMove :: Game -> Int -> Game                                                 -- generate a new random move and update game
makeMove game seed = do
  let turn = checkTurn game
  let gamePieces = if (turn == Blue)                                            -- specify gamepieces to update (red or blue?)
                  then (gamePiecesBlue game)
                  else (gamePiecesRed game)

  let playerCards = if (turn == Blue)                                           -- available players cards for each player
                  then [] ++ [(gameCards game) !! 0] ++ [(gameCards game) !! 1]
                  else [] ++ [(gameCards game) !! 2] ++ [(gameCards game) !! 3]

  let card = chooseRandomCard (mkStdGen seed)
  let possibleTurns = if (  (gameTurn game) == Blue)                             -- set blue or red move possibilities
                    then turnsPossible possibleMovesBlue card playerCards
                    else turnsPossible possibleMovesRed card playerCards

  let piece = gamePieces !! (chooseRandomPiece (length gamePieces) (mkStdGen seed) )
  let move = possibleTurns !! (chooseRandomMove (length possibleTurns)   (mkStdGen seed) )
  let verifyMove = checkLegalMove gamePieces ((fst piece) + (fst move), (snd piece) + (snd move))

  let theMove = if (verifyMove == True)                                                 -- if current move is not legal, then we try with another
                then (move, (playerCards !! card))
                else do
                  let mv = checkLegalGenerate seed possibleTurns gamePieces move
                  if (mv /= move)                                               -- if mv is not initial move, it means we have found another legal move alternative with this card.
                    then (mv, (playerCards !! card))
                    else do                                                     -- this means that the initial move was not legal, and no legal move was left for this card. We try the other.
                      let newCard = removeItem (playerCards !! card) playerCards -- size of this can only be one  (index 0) as we "remove" other card
                      let possibleTurns2 = if ( ( gameTurn game) == Blue )
                                          then turnsPossible possibleMovesBlue 0 newCard
                                          else turnsPossible possibleMovesRed 0 newCard
                      let move2 = possibleTurns2 !! (chooseRandomMove (length possibleTurns2)  (mkStdGen seed) )
                      let verifyMove2 = checkLegalMove gamePieces ((fst piece) + (fst move2), (snd piece) + (snd move2))
                      if (verifyMove2)
                        then (move2, (newCard !! 0))
                        else (checkLegalGenerate seed possibleTurns2 gamePieces move2, (newCard !! 0))

  -- MAKE SOME FUNCTION FOR OTHER CARD TMRW
  let tupMove = (fst theMove)
  let actualCard = (snd theMove)

  let newPiece = if (checkLegalMove gamePieces ((fst piece)+(fst tupMove), (snd piece)+(snd tupMove)))
                then ((fst piece)+(fst tupMove), (snd piece)+(snd tupMove))
                else piece                                                      -- if the move is still not legal, we will not perform any move


  let newGamePieces = removeItem piece gamePieces

  let fullMove = (piece, newPiece, actualCard)
                                                                                -- update game variables
  let initialWPieces = updatePieces game (chooseRandomPiece (length gamePieces) (mkStdGen seed) ) newGamePieces newPiece
  let initialOpPiece = takePiece initialWPieces newPiece
  let initialWCards = updateCards initialOpPiece actualCard playerCards
  let initialWTurn = switchPlayer initialWCards
  let newGame = updateMove initialWTurn fullMove
  newGame


checkLegalGenerate :: Int -> PieceList -> PieceList -> Coordinate -> Coordinate -- Finds a legal move if current move is not legal.
checkLegalGenerate seed xyz xy mv = do
  let legalMove = checkLegalMove xy mv
  let move = if (legalMove == True)
            then mv
            else do                                                             -- If legalMove is false, remove the move from possibleTurns array, and try again
              let arrLength = (length xyz)                                      -- If array is of length 1, there is only one other possibility
              if (arrLength == 1)
                then (head xyz)
                else do
                  let newArr = removeItem mv xyz
                  let newMv = newArr !! (chooseRandomMove (length newArr) (mkStdGen seed) )
                  checkLegalGenerate seed newArr xy newMv
  move


checkLegalMove :: PieceList -> Coordinate -> Bool
checkLegalMove xy mv                                                            -- If the move is already in the pieceList, then we cannot make this move, because else we will have 2 pieces on same spot.
  | (fst mv) > 4 = False                                                        -- check if xy out of bounds
  | (fst mv) < 0 = False                                                        --------
  | (snd mv) > 4 = False                                                        --------
  | (snd mv) < 0 = False
  | (mv `elem` xy) = False                                                        --------
  | otherwise = True


updateCards :: Game -> String -> [Card] -> Game
updateCards gm cardName cards = do
  let arr = removeItem cardName cards
  let newPlayerCards = sort (arr ++ [(last(gameCards gm))])
  let opponentCards = if ((gameTurn gm) == Blue)
                   then sort ([] ++ [(gameCards gm) !! 2] ++ [(gameCards gm) !! 3])
                   else sort ([] ++ [(gameCards gm) !! 0] ++ [(gameCards gm) !! 1])
  let newArray = if ((gameTurn gm) == Blue)
                then newPlayerCards ++ opponentCards ++ [cardName]
                else opponentCards ++ newPlayerCards ++ [cardName]

  gm { gameCards = newArray }


turnsPossible :: CardMoves -> Int -> [String] -> [(Int, Int)]
turnsPossible possibleMoves n arr
  | (arr !! n) == "Cobra" = (cobra possibleMoves)
  | (arr !! n) == "Rabbit" = (rabbit possibleMoves)
  | (arr !! n) == "Rooster" = (rooster possibleMoves)
  | (arr !! n) == "Tiger" = (tiger possibleMoves)
  | otherwise = (monkey possibleMoves)

takePiece :: Game -> Coordinate -> Game
takePiece gm xy = do
  let opponentXY = if ( (gameTurn gm) == Blue)
                  then (gamePiecesRed gm)
                  else (gamePiecesBlue gm)
  let removePiece = if ( (xy `elem` opponentXY) == True)
                    then xy
                    else (99,99)
  let senseiRemove = if ((opponentXY !! 0) == removePiece)
                      then True
                      else False
  let newOpponentXY = removeItem removePiece opponentXY
  setOpponentXY gm senseiRemove newOpponentXY

setOpponentXY :: Game -> Bool -> [Coordinate] -> Game
setOpponentXY gm sensei arr
  | ((gameTurn gm) == Blue && sensei == True) = gm {gamePiecesRed = arr, gameState = GameOver, winner = Blue} -- sensei was killed, set winner to whoevers turn it is
  | ((gameTurn gm) == Red && sensei == True) = gm {gamePiecesBlue = arr, gameState = GameOver, winner = Red}
  | ((gameTurn gm) == Blue) = gm {gamePiecesRed = arr}
  | ((gameTurn gm) == Red) = gm {gamePiecesBlue = arr}


updatePieces :: Game -> Int -> [(Int,Int)] -> (Int, Int) -> Game
updatePieces gm ind arr newPiece
  | ind == 0 && (gameTurn gm == Blue) =
    gm { gamePiecesBlue = newPiece:(sort(arr)) }
  | ind == 0 && (gameTurn gm == Red) =
    gm { gamePiecesRed = newPiece:(sort(arr)) }
  | ind /= 0 && (gameTurn gm == Blue) =
    gm { gamePiecesBlue = ([head arr] ++ sort(tail(arr) ++ [newPiece])) }
  | ind /= 0 && (gameTurn gm == Red) =
    gm { gamePiecesRed = ([head arr] ++ sort(tail(arr) ++ [newPiece])) }

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

----------------------------------------------
----------- MAKE INITIAL TABLE ---------------
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
      string5 = (newList4 !! 0)                                                 -- size will be 1 and we will get last element available
  in  [string1, string2, string3, string4, string5]

finalizeArr :: [String] -> [String]
finalizeArr x = ( (sort([x !! 0] ++ ([x !! 1]))) ++ (sort([x !! 2] ++ ([x !! 3]))) ++ [x !! 4])

---------- Remove item in array -------------
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys
---------------------------------------------
-------------- PRINTERS ---------------------
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
--- TESTER ---
initt = Game { gameCards = randomInitial (mkStdGen 100)
                    , gameTurn = Blue
                    , gamePiecesBlue = [(0,2),(0,0),(0,1),(0,3),(0,4)]
                    , gamePiecesRed = [(4,2),(4,0),(4,1),(4,3),(4,4)]
                    , recentMove = ((0,0),(0,0),"null")
                    , gameState = Running
                    , winner = None}

lol1 = makeMove initt 100
lol2 = makeMove lol1 100
lol3 = makeMove lol2 100
lol4 = makeMove lol3 100
lol5 = makeMove lol4 100
lol6 = makeMove lol5 100
lol7 = makeMove lol6 100
lol8 = makeMove lol7 100
lol9 = makeMove lol8 100
lol10 = makeMove lol9 100
lol11 = makeMove lol10 100
lol12 = makeMove lol11 100


-------------------------------------------------------
--------------- FUNCTIONALITY -------------------------
generateRandom :: Int -> Int -> IO (String)
generateRandom x y = do
  let game = Game { gameCards = finalizeArr (randomInitial (mkStdGen x))
                      , gameTurn = Blue
                      , gamePiecesBlue = [(0,2),(0,0),(0,1),(0,3),(0,4)]
                      , gamePiecesRed = [(4,2),(4,0),(4,1),(4,3),(4,4)]
                      , recentMove = ((0,0),(0,0),"null")
                      , gameState = Running
                      , winner = None}

  let initialTable = printTable game
  let game1 = makeMove game x
  let total = gameLoop x y game1 ""                                             -- append to empty string
  return (initialTable ++ total)

-------------------------------------------------------
------ GAMELOOP FOR GENERATING RANDOM GAMES -----------
gameLoop :: Int -> Int -> Game -> String -> String
gameLoop _ 0 _ s = s                                                            -- return final string when n==0
gameLoop seed n gm string  | (gameState gm) == GameOver = "Game Over."
                           | (winner gm) == Blue = "Blue Wins."
                           | (winner gm) == Red = "Red Wins."
                           | otherwise = do

    let move = printMove gm
    let newString = string ++ move
    let newGame = makeMove gm seed
    gameLoop (seed) (n-1) newGame newString
----------------------------------------------------------

isValid :: FilePath -> IO (String)
isValid filePath = do
  strings <- readFile filePath
  return strings


hasWinningStrategy :: Int -> FilePath -> IO (String)
hasWinningStrategy _ _ = return "Not yet implemented"
