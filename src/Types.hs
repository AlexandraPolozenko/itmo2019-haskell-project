module Types (
    World(..)
  , Field(..)
  , Card(..)
  , Combo(..)
  , StateChanges(..)
  , ClientState(..)
  , Player(..)
  , TurnState(..)
  , Suit(..)
  , FieldState(..)
  , Command(..)
  , Changes(..)
  , Turn(..)
  , messageSize
  , defaultCard
  ) where

import Network.Socket (Socket(..))

data World = World [Field] [Card] [StateChanges]
  deriving (Show, Eq)

data ClientState = ClientState Player Socket [Field] [Card] TurnState
  deriving (Show, Eq)

data TurnState = PutCardTurn (Maybe Card) | EmptyState | GameFinished Player
  deriving (Show, Eq)

data Card = Card Suit Int
  deriving (Show, Eq)

data Suit = Red | Blue | Yellow | Green | White | Black
  deriving (Show, Eq)

data Field = Field [Card] [Card] FieldState
  deriving (Show, Eq)

data FieldState = Closed Player | Open
  deriving (Show, Eq)

data Player = One | Two
  deriving (Show, Eq)

data Command = Put | Take Card | Win Player
  deriving (Show, Eq)

data StateChanges = NewCard Int Player Card | FieldClosed Int Player | Winner Player
  deriving (Show, Eq)

data Changes = Changes [StateChanges]
  deriving (Show, Eq)

data Turn = PutCard Int Player Card | FinishTurn | TakeCard Card
  deriving (Show, Eq)

data Combo = Host Int | Skirmish Int | Batallion Int | Phalanx Int | Wedge Int
  deriving (Show, Eq, Ord)

messageSize = 10000 :: Int

defaultCard = (Card Black 0) :: Card

portNumber = 5005 :: Int