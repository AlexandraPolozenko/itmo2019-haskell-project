module ByteStringParser where

import Types
import qualified Data.Binary as B (Binary, Get, put, get, putList, decode, encode)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Internal as BB
import Data.Word8


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
      2 -> do
        p <- B.get
        return $ Winner p

instance B.Binary Changes where
  put (Changes c) = do
    B.putList c
  get = do
    t <- B.get
    return $ Changes t

instance B.Binary Turn where
  put (PutCard f p c) = do
    B.put (0 :: Word8)
    B.put f
    B.put p
    B.put c
  put (TakeCard c) = do
    B.put (1 :: Word8)
    B.put c
  put (MakeProof f c) = do
    B.put (2 :: Word8)
    B.put f
    B.putList c
  put FinishTurn = do
  	B.put (3 :: Word8)
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
      2 -> do
        f <- B.get
        c <- B.get
        return $ MakeProof f c
      3 -> return FinishTurn


instance B.Binary Command where
  put Put = do
    B.put (0 :: Word8)
  put Proof = do
    B.put (1 :: Word8)
  put (Take c) = do
    B.put (2 :: Word8)
    B.put c
  put (Win p) = do
    B.put (3 :: Word8)
    B.put p
  get = do
    t <- B.get :: B.Get Word8
    case t of
      0 -> return Put
      1 -> return Proof
      2 -> do
        c <- B.get
        return $ Take c
      3 -> do
        p <- B.get
        return $ Win p

instance B.Binary Player where
  put One = do
    B.put (1 :: Word8)
  put Two = do
    B.put (2 :: Word8)
  get = do
    t <- B.get :: B.Get Word8
    case t of
      1 -> return One
      2 -> return Two


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
      5 -> do
        i <- B.get
        return $ Card Black i


encode :: B.Binary a => a -> BB.ByteString
encode b = BS.toStrict $ B.encode b

decodeTurn :: BB.ByteString -> Turn
decodeTurn b = (B.decode $ BS.fromStrict b) :: Turn

decodeCommand :: BB.ByteString -> Command
decodeCommand b = (B.decode $ BS.fromStrict b) :: Command

decodePlayer :: BB.ByteString -> Player
decodePlayer a = (B.decode $ BS.fromStrict a) :: Player

decodeCard :: BB.ByteString -> Card
decodeCard a = (B.decode $ BS.fromStrict a) :: Card

decodeCards :: BB.ByteString -> [Card]
decodeCards a = (B.decode $ BS.fromStrict a) :: [Card]

decodeChanges :: BB.ByteString -> Changes
decodeChanges a = (B.decode $ BS.fromStrict a) :: Changes