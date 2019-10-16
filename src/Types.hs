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
import Data.Word8 (Word8)
import qualified Data.Binary as B (Binary, Get, put, get, putList)


messageSize :: Int
messageSize = 10000

defaultCard :: Card
defaultCard = (Card Black 0)

data World = World [Field] [Card] [StateChanges]
  deriving (Show, Eq)

data ClientState = ClientState Player Socket [Field] [Card] TurnState
  deriving (Show, Eq)

data TurnState = PutCardTurn (Maybe Card) | EmptyState | GameFinished Player
  deriving (Show, Eq)

data Suit = Red | Blue | Yellow | Green | White | Black
  deriving (Show, Eq)

data Field = Field [Card] [Card] FieldState
  deriving (Show, Eq)

data FieldState = Closed Player | Open
  deriving (Show, Eq)

data Combo = Host Int | Skirmish Int | Batallion Int | Phalanx Int | Wedge Int
  deriving (Show, Eq, Ord)

data StateChanges = NewCard Int Player Card | FieldClosed Int Player | Winner Player
  deriving (Show, Eq)

instance B.Binary StateChanges where
  put (NewCard f p c) = do
    B.put (0 :: Word8)
    B.put f
    B.put p
    B.put c
  put (FieldClosed f p) = do
    B.put (1 :: Word8)
    B.put f
    B.put p
  put (Winner p) = do
    B.put (2 :: Word8)
    B.put p
  get = do
    t <- B.get :: B.Get Word8
    case t of 
      0 -> do
        f <- B.get
        p <- B.get
        c <- B.get
        return $ NewCard f p c
      1 -> do
        f <- B.get
        p <- B.get
        return $ FieldClosed f p
      _ -> do
        p <- B.get
        return $ Winner p


data Changes = Changes [StateChanges]
  deriving (Show, Eq)

instance B.Binary Changes where
  put (Changes c) = do
    B.putList c
  get = do
    t <- B.get
    return $ Changes t


data Turn = PutCard Int Player Card | FinishTurn | TakeCard Card
  deriving (Show, Eq)

instance B.Binary Turn where
  put (PutCard f p c) = do
    B.put (0 :: Word8)
    B.put f
    B.put p
    B.put c
  put (TakeCard c) = do
    B.put (1 :: Word8)
    B.put c
  put FinishTurn = do
    B.put (2 :: Word8)
  get = do
    t <- B.get :: B.Get Word8
    case t of
      0 -> do
        f <- B.get
        p <- B.get
        c <- B.get
        return $ PutCard f p c
      1 -> do
        c <- B.get
        return $ TakeCard c
      _ -> return FinishTurn


data Command = Put | Take Card | Win Player | EmptyCommand
  deriving (Show, Eq)

instance B.Binary Command where
  put Put = do
    B.put (0 :: Word8)
  put (Take c) = do
    B.put (1 :: Word8)
    B.put c
  put (Win p) = do
    B.put (2 :: Word8)
    B.put p
  put EmptyCommand = do
    B.put (3 :: Word8)
  get = do
    t <- B.get :: B.Get Word8
    case t of
      0 -> return Put
      1 -> do
        c <- B.get
        return $ Take c
      2 -> do
        p <- B.get
        return $ Win p
      _ -> return EmptyCommand


data Player = One | Two
  deriving (Show, Eq)

instance B.Binary Player where
  put One = do
    B.put (1 :: Word8)
  put Two = do
    B.put (2 :: Word8)
  get = do
    t <- B.get :: B.Get Word8
    case t of
      1 -> return One
      _ -> return Two

data Card = Card Suit Int
  deriving (Show, Eq)

instance B.Binary Card where
  put (Card Red i) = do
    B.put (0 :: Word8)
    B.put i
  put (Card Blue i) = do
    B.put (1 :: Word8)
    B.put i
  put (Card Yellow i) = do
    B.put (2 :: Word8)
    B.put i
  put (Card Green i) = do
    B.put (3 :: Word8)
    B.put i
  put (Card White i) = do
    B.put (4 :: Word8)
    B.put i
  put (Card Black i) = do
    B.put (5 :: Word8)
    B.put i
  get = do
    t <- B.get :: B.Get Word8
    case t of
      0 -> do
        i <- B.get
        return $ Card Red i
      1 -> do
        i <- B.get
        return $ Card Blue i
      2 -> do
        i <- B.get
        return $ Card Yellow i
      3 -> do
        i <- B.get
        return $ Card Green i
      4 -> do
        i <- B.get
        return $ Card White i
      _ -> do
        i <- B.get
        return $ Card Black i
