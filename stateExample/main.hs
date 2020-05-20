import System.IO
import Control.Monad
import Control.Monad.State
import System.Random
import Data.Array.IO

data Player = Player1 | Player2 deriving (Eq, Show)

data GameState = Game
  { players :: [Player]
  , chanceDeck :: [GameCard]
  , generator :: StdGen } deriving (Show)

initial :: GameState
initial = Game {players = [Player1, Player2], chanceDeck = ["Pikachu", "Raichu"], generator = (mkStdGen 100)}

type GameCard = String

rollDice :: State GameState Int
rollDice = do
  currentState <- get
  let gen = generator currentState
  let (d1, gen') = randomR (1,6) gen
  let (d2, gen'') = randomR (1,6) gen'
  put (currentState { generator = gen''} )
  return (d1 + d2)


resolveTurn :: State GameState ()
resolveTurn = do
  currentState <- get
  roll <- rollDice
  return ()
