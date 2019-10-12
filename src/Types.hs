module Types where

data World = World [Field] [Card]
  deriving (Show, Eq)

data ClientState = ClientState Player [Field] [Card]
  deriving (Show, Eq)

data Card = Card Suit Int
  deriving (Show, Eq)

data Suit = Red | Blue | Yellow | Green | White | Black
  deriving (Show, Eq)

data Field = Field [Card] [Card] FieldState
  deriving (Show, Eq)

data FieldState = Player | Open
  deriving (Show, Eq)

data Player = One | Two
  deriving (Show, Eq)

-- data Combination = Combination [Card]
--   deriving (Show, Eq)

data Turn = PutCard Int Player Card | Win Player | Proof [Card] | FinishTurn | MakeTurn | TakeCard Card
  deriving (Show, Eq)

-- data TurnState = Player | Okay | Error
--   deriving (Show, Eq)

fieldSize = 9 :: Int