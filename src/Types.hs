module Types where

import Network.Socket

data World = World [Field] [Card]
  deriving (Show, Eq)

data ClientState = ClientState Player Socket [Field] [Card] TurnState
  deriving (Show, Eq)

data TurnState = PutCardTurn (Maybe Card) | MakeProofTurn (Maybe Int) [Card] | EmptyState | Start
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

-- data Combination = Combination [Card]
--   deriving (Show, Eq)

data Command = Put | Proof | Take Card | Win Player
  deriving (Show, Eq)

data StateChanges = NewCard Int Player Card | FieldClosed Int Player | Winner Player
  deriving (Show, Eq)

data Changes = Changes [StateChanges]
  deriving (Show, Eq)

data Turn = PutCard Int Player Card | MakeProof Int [Card] | FinishTurn | TakeCard Card
  deriving (Show, Eq)

-- data TurnState = Player | Okay | Error
--   deriving (Show, Eq)

fieldSize = 9 :: Int