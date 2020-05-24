# my-project
For representing a game i have chosen to create the datatype:  
```haskell
data Game = Game { gameCards :: [Card]
                  , gameTurn :: Player
                  , gamePiecesBlue :: PieceList
                  , gamePiecesRed :: PieceList
                  , recentMove :: Move
                  , gameState :: Status
                  , winner :: Player
                  } deriving (Eq, Show)
```  
This type will help me keep track of the changing values in the game as they should be updated.  
For generating the actual games, i made a
```haskell
gameLoop :: Int -> Int -> Game -> String -> String
```
that will iterate n times, and then finally returning an appended string, represent all of the moves. The random seed will be incremented by 1 for every iteration, to help increase randomness.  
Following types have also been created:
```haskell
data Player = Blue | Red | None deriving (Eq, Show)
data Status = Running | GameOver deriving (Eq, Show)


type Move = ((Int,Int), (Int, Int), [Char])
type PieceList = [Coordinate]
type Coordinate = (Int, Int)
type Card = String
type GameOV = ([Card], PieceList, PieceList)
```  
These were created for more simplicity and readability.  

Also, I made a data type for holding the available moves for the cards:  
```haskell
data CardMoves = CardMoves { cobra :: [Coordinate]
                            , rabbit :: [Coordinate]
                            , rooster :: [Coordinate]
                            , tiger :: [Coordinate]
                            , monkey :: [Coordinate]
                            } deriving (Eq, Show)

possibleMovesBlue = CardMoves { cobra = [(1,0), (1,1), (1,-1)]
                              , rabbit = [(-1,-1), (1,1), (2,0)]
                              , rooster = [(-1,-1), (-1,0), (1,0), (1,1)]
                              , tiger = [(0,2), (0,-1)]
                              , monkey = [(-1,1), (1,1), (-1,-1), (1,-1)]}
```  
These helps the program check if the move the player is about to make, is legal using the particular card.  
  
Currently, the reason I am not hitting 100% test coverage, is because my current tests does not reach a random seed that causes 
```haskell
moveSensei
``` to be true, and also I have not implemented hasWinningStrategy as of yet.
