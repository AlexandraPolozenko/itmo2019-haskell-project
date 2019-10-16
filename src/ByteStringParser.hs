module ByteStringParser(
    encode
  , decodeTurn
  , decodeCommand
  , decodePlayer
  , decodeCard
  , decodeCards
  , decodeChanges
  ) where

import qualified Data.Binary as B (Binary, decode, encode)
import qualified Data.ByteString.Lazy as BS (toStrict, fromStrict)
import Data.ByteString.Internal as BB (ByteString(..))

import Types (Changes(..), Turn(..), Command(..), Player(..), Card(..))


encode :: B.Binary a => a -> BB.ByteString
encode b = BS.toStrict $ B.encode b

decodeTurn :: BB.ByteString -> Turn
decodeTurn b = (B.decode $ BS.fromStrict b) :: Turn

decodeCommand :: Maybe BB.ByteString -> Command
decodeCommand Nothing = EmptyCommand
decodeCommand (Just b) = (B.decode $ BS.fromStrict b) :: Command

decodePlayer :: BB.ByteString -> Player
decodePlayer a = (B.decode $ BS.fromStrict a) :: Player

decodeCard :: BB.ByteString -> Card
decodeCard a = (B.decode $ BS.fromStrict a) :: Card

decodeCards :: BB.ByteString -> [Card]
decodeCards a = (B.decode $ BS.fromStrict a) :: [Card]

decodeChanges :: Maybe BB.ByteString -> Changes
decodeChanges Nothing = Changes [] 
decodeChanges (Just a) = (B.decode $ BS.fromStrict a) :: Changes