module Lib (
 generateRandom,
 isValid,
 hasWinningStrategy
 ) where

import System.IO
import Control.Monad
import System.Random
import Data.List


data Player = Blue | Red | None deriving (Eq)
data Status = Running | GameOver deriving (Eq)


type Move = ((Int,Int), (Int, Int), [Char])
type PieceList = [Coordinate]
type Coordinate = (Int, Int)
type Card = String
type GameOV = ([Card], PieceList, PieceList)

---- pattern matching for Move and GameOV type
fst' (a,_,_) = a
snd' (_,b,_) = b
thd' (_,_,c) = c
------------------------------------

data CardMoves = CardMoves { cobra :: [Coordinate]
                            , rabbit :: [Coordinate]
                            , rooster :: [Coordinate]
                            , tiger :: [Coordinate]
                            , monkey :: [Coordinate]
                            }

data Game = Game { gameCards :: [Card]
                  , gameTurn :: Player
                  , gamePiecesBlue :: PieceList
                  , gamePiecesRed :: PieceList
                  , recentMove :: Move
                  , gameState :: Status
                  , winner :: Player
                  }


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

  let moveSensei = if ((gamePieces !! 0 ) == piece )                            -- if the piece is index 0, we are moving the sensei
                    then True
                    else False


  let move = possibleTurns !! (chooseRandomMove (length possibleTurns)   (mkStdGen seed) )
  let verifyMove = checkLegalMove gamePieces ((fst piece) + (fst move), (snd piece) + (snd move))

  let theMove = if (verifyMove == True)                                         -- if current move is not legal, then we try with another
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

  let tupMove = (fst theMove)
  let actualCard = (snd theMove)

  let newPiece = if (checkLegalMove gamePieces ((fst piece)+(fst tupMove), (snd piece)+(snd tupMove)))
                then ((fst piece)+(fst tupMove), (snd piece)+(snd tupMove))
                else piece                                                      -- if the move is still not legal, we will not perform any move

  let newGamePieces = removeItem piece gamePieces

  let fullMove = (piece, newPiece, actualCard)
                                                                                -- update game variables
  let initialWPieces = updatePieces game (chooseRandomPiece (length gamePieces) (mkStdGen seed) ) newGamePieces newPiece
  let initialOpPiece = takePiece initialWPieces newPiece -- update opponent pieces
  let initialWCards = updateCards initialOpPiece actualCard playerCards -- update cards
  let wayOfStream = wayOfTheStream initialOpPiece -- check way of the stream
  let wayOfTheStream = if (wayOfStream == True)
                      then initialWCards { gameState = GameOver, winner = turn}
                      else initialWCards
  let initialWTurn = switchPlayer wayOfTheStream
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
  | (mv `elem` xy) = False                                                      --------
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

wayOfTheStream :: Game -> Bool
wayOfTheStream gm = do
  let turn = (gameTurn gm)
  let pieceList = if (turn == Blue)
                  then (gamePiecesBlue gm)
                  else (gamePiecesRed gm)
  let senseiPiece = (pieceList !! 0)
  let winCond = if (turn == Blue)
                then (4,2)
                else (0,2)
  let wayOfStream = if (senseiPiece == winCond)
                    then True
                    else False
  wayOfStream


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
      string1 = (["Cobra", "Rabbit", "Rooster", "Tiger", "Monkey"] !! int1)
      newList = removeItem string1 ["Cobra", "Rabbit", "Rooster", "Tiger", "Monkey"]
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
  "(" ++ show game ++ "," ++ show piecesP1 ++ "," ++ show piecesP2 ++ ")" ++ "\n"


printMove :: Game -> String
printMove table = do
  let move = if (gameTurn table == Blue)
            then alternateMove (recentMove table)
            else (recentMove table)
  show move ++ "\n"
-------------------------------------------------------


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
gameLoop _ 0 gm s = s ++ printMove gm                                           -- return final string when n==0, remember to print final move.
gameLoop seed n gm string  | (gameState gm) == GameOver = do
                              --let finalString = string ++ printMove gm ++ "Game Over!\n"
                              --let winnerString = if ((winner gm) == Blue)
                                        --        then "Blue"
                                            --    else "Red"
                              --let moveWin = winnerString ++ " made the winning-move: " ++ printMove gm ++ printTable gm (not used for final product cause they dont represent valid output)
                              string ++ printMove gm
                           | otherwise = do
                              --let table = printTable gm -- for debugging
                              let move = printMove gm-- ++ table
                              let newString = string ++ move
                              let newGame = makeMove gm seed
                              gameLoop (seed+1) (n-1) newGame newString                                   -- increment every iteration with some number(+1 right now), to get different seeds and increase randomness!
----------------------------------------------------------

isValid :: FilePath -> IO (String)
isValid filePath = do
  content <- readFile filePath
  let linesOfContent = lines content
  -- check the first line if table is good
  let tableValid = if (checkInitial (linesOfContent !! 0))
                    then True
                    else False
  let moves = delete (linesOfContent !! 0) linesOfContent                       --- Cutting off initial lines and checking it for itself.
  let i = length (moves)
  if (tableValid == False)
    then return "ParsingError"
    else do
        let table = (read (linesOfContent !! 0) :: GameOV)
        let game = Game {gameCards = (fst' table)
                        ,gamePiecesBlue = (snd' table)
                        ,gamePiecesRed = (thd' table)
                        ,gameTurn = Blue
                        ,recentMove = ((0,0),(0,0),"null")
                        ,gameState = Running
                        ,winner = None}
        let wayOfStream = wayOfTheStream game
        let wayOfStreamWinner = if (wayOfStream == True)
                                then game {gamePiecesRed = []}
                                else game
        let result = if (wayOfStream == True)
                    then "(" ++ show (gameCards wayOfStreamWinner) ++ "," ++ show (gamePiecesBlue wayOfStreamWinner) ++ "," ++ show (gamePiecesRed wayOfStreamWinner) ++ ")"
                    else isValidFunc i moves game
        return result

-- (1,3) =

printTable2 :: Game -> String                                                   -- needed this to pass extra tests on website..
printTable2 table = do
  let game = (gameCards table)
  let piecesP1 = (gamePiecesBlue table)
  let piecesP2 =  (gamePiecesRed table)
  let perspective = if ((gameTurn table) == Blue)                               -- opposite because we switch player before checking for end of loop ..
                    then "(" ++ show game ++ "," ++ show piecesP1 ++ "," ++ show piecesP2 ++ ")"
                    else "(" ++ show (alternatePerspectiveCards game) ++ "," ++ show (constructPerspective piecesP2) ++ "," ++ show (constructPerspective piecesP1) ++ ")"  -- need to print from winners perspective
  perspective

alternatePerspective :: PieceList -> PieceList                                                  -- sets the right perspective. mirrors the coordinates.
alternatePerspective [] = []
alternatePerspective (x:xs) =  [((4 - (fst x)), 4 - (snd x))] ++ alternatePerspective xs

alternatePerspectiveCards :: [Card] -> [Card]
alternatePerspectiveCards xy = ([xy !! 2] ++ [xy !! 3] ++ [xy !! 0] ++ [xy !! 1] ++ [xy !! 4])

alternateMove :: Move -> Move
alternateMove xy = do
  let firstCoord = (4 - fst(fst' xy), 4 - snd (fst' xy))
  let secondCoord = (4 - fst(snd' xy), 4 - snd (snd' xy))
  (firstCoord, secondCoord, thd' xy)

constructPerspective :: PieceList -> PieceList                                  -- we need to make sure it is also sorted lexicographically
constructPerspective xs = do
  let newList = alternatePerspective xs
  let sorted = if (newList /= [])
              then [head newList] ++ sort (tail newList)
              else []
  sorted

isValidFunc :: Int -> [String] -> Game -> String
isValidFunc 0 _ gm | (gameState gm) == GameOver = do
                            let winnerr = (winner gm)
                            let finalGame = if (winnerr == Blue)
                                              then gm {gamePiecesRed = []}
                                              else gm {gamePiecesBlue = []}
                            printTable2 finalGame
                    | otherwise = printTable2 gm
isValidFunc n content gm = do

  let str = (content !! 0) -- do some rule checking on this string.
  let newArr = delete str content
  let move = if (gameTurn gm == Blue)
              then read str :: Move
              else alternateMove (read str :: Move)
  let gamePieces = if ((gameTurn gm) == Blue)                                            -- specify gamepieces to update (red or blue?)
                  then (gamePiecesBlue gm)
                  else (gamePiecesRed gm)
  let playerCards = if (gameTurn gm == Blue)                                           -- available players cards for each player
                  then [] ++ [(gameCards gm) !! 0] ++ [(gameCards gm) !! 1]
                  else [] ++ [(gameCards gm) !! 2] ++ [(gameCards gm) !! 3]
  let senseimove = if ((fst' move) == (gamePieces !! 0))
                  then 0
                  else 1

  let moveSensei = if (senseimove == 0)
                  then True
                  else False

  let newGamePieces = removeItem (fst' move) gamePieces

  let validMove = if (gameTurn gm == Red)
                  then checkValidMove gm gamePieces newGamePieces move
                  else checkValidMove gm gamePieces newGamePieces move


  if (validMove == False)
    then ("NonValid " ++ str)
    else do
      let verifyMove = if ((snd' move) `elem` gamePieces)
                    then (fst' move)
                    else (snd' move)
      let newGame = updatePieces gm senseimove newGamePieces verifyMove
      let newGame' = takePiece newGame verifyMove
      let newGame'' = updateCards newGame' (thd' move) playerCards
      let wayOfStream = wayOfTheStream newGame'
      let wayOfTheStream = if (wayOfStream == True)
                          then newGame'' { gameState = GameOver, winner = (gameTurn gm)}
                          else newGame''
      let newGame''' = switchPlayer wayOfTheStream
      isValidFunc (n-1) newArr newGame'''

----------------------------------------------------------------------------
------------------CHECKERS FOR ISVALID--------------------------------------


checkForMissing :: GameOV -> Bool
checkForMissing gm = do
  --let checkRedPieces = if ((thd' gm) /= [(4,2),(4,0),(4,1),(4,3),(4,4)]) --- This was outcommented as i found out it is OK to have middle-game state as initial game :D
      --                  then False
    --                    else True
  --let checkBluePieces = if ((snd' gm) /= [(0,2),(0,0),(0,1),(0,3),(0,4)])
                --        then False
                --        else True
  --if ((checkRedPieces == True) && (checkBluePieces == True))
    --then True
    --else False
  let checkRedPiece = if (length (thd' gm) > 5)
                      then False
                      else True

  let checkBluePiece = if (length (snd' gm) > 5)
                      then False
                      else True
  if ((checkRedPiece == True) && (checkBluePiece == True))
    then True
    else False



checkValidMove :: Game -> PieceList -> PieceList -> Move -> Bool
checkValidMove gm oldxy newxy mv = do
  let checkOOBExisting = if (checkLegalMove newxy (snd' mv) == False)                  --- checkLegalMove function also used for generating random moves.
                          then False
                          else True


  let possibleTurns = if (gameTurn gm == Blue)
                      then turnsPossible2 possibleMovesBlue (thd' mv)
                      else turnsPossible2 possibleMovesRed (thd' mv)


  let cardMove = (((fst (snd' mv)) - fst (fst' mv)), (snd (snd' mv) - snd (fst' mv)))  --- Find what the move was, by subtracting end position from start position. Then check if is in possible moves for that card.
  let checkValidCardMove = if (cardMove `elem` (possibleTurns) || cardMove == (0,0))   --- If the cardMove is (0,0) it jsut means we did not make a move and stayed at position.
                          then True
                          else False
  let availableCards = if ((gameTurn gm) == Blue)                                 --- Check if the card the player wants to use is actually on the players hand
                        then ([(gameCards gm) !! 0] ++ ([(gameCards gm) !! 1]))
                        else ([(gameCards gm) !! 2] ++ ([(gameCards gm) !! 3]))

  let cardOnHand = if ((thd' mv) `elem` availableCards)
                    then True
                    else False

  let validPiece = if ((fst' mv) `elem` oldxy)                                  -- check if we axtually have a piece to move heere
                  then True
                  else False

  let checkGameEnded = if (gameState gm == GameOver)
                      then True
                      else False

  if ( (checkOOBExisting == True) && (checkValidCardMove == True) && (possibleTurns /= []) &&
        (validPiece == True) && (cardOnHand == True) && (checkGameEnded == False)) --- if posibleTurns == [], then an unknown card has been specified in moves.
    then True
    else False

turnsPossible2 :: CardMoves -> String -> [(Int, Int)]                           --- same as from makeMove but just checks for string instead of index
turnsPossible2 possibleMoves str
  | str == "Cobra" = (cobra possibleMoves)
  | str == "Rabbit" = (rabbit possibleMoves)
  | str == "Rooster" = (rooster possibleMoves)
  | str == "Tiger" = (tiger possibleMoves)
  | str == "Monkey" = (monkey possibleMoves)
  | otherwise = []


checkInitial :: String -> Bool
checkInitial string = do
  let gameTable = read string :: GameOV
  if (checkValidGame gameTable)
    then True
    else False

checkSameStart :: PieceList -> Bool
checkSameStart []                 = False
checkSameStart (x:xs)   | x `elem` xs   = True
                        | otherwise = checkSameStart xs

checkBoundaries :: PieceList -> Bool
checkBoundaries []          = False
checkBoundaries (x:xs)  | (fst x) > 4 = True
                        | (snd x) > 4 = True
                        | (fst x) < 0 = True
                        | (snd x) < 0 = True
                        | otherwise = checkBoundaries xs


checkValidGame :: GameOV -> Bool                                                -- Checks if the initial table is in lexicographical order as well as the piecelists.
checkValidGame game = do                                                        -- if not the game is not valid and we should return ParseError.
  let cards = fst' game
  let finTable = finalizeArr cards
  let pieceLists = (snd' game) ++ (thd' game)
  let checkSameStartPos = checkSameStart pieceLists
  let checkBounds1 = checkBoundaries (snd' game)
  let checkBounds2 = checkBoundaries (thd' game)
  let validtable = if (cards == finTable)
                  then True
                  else False
  let pieceList1 = snd' game
  let validP1 = if (pieceList1 == ([head pieceList1] ++ sort(tail(pieceList1))))
                then True
                else False
  let pieceList2 = thd' game
  let validP2 = if (pieceList2 == ([head pieceList2] ++ sort(tail(pieceList2))))
                then True
                else False
  let missingPieces = checkForMissing game
  if (validtable == True && validP1 == True && validP2 == True && missingPieces == True
     && checkSameStartPos == False && checkBounds1 == False && checkBounds2 == False)
    then True
    else False


hasWinningStrategy :: Int -> FilePath -> IO (String)
hasWinningStrategy _ _ = return "Not yet implemented"
