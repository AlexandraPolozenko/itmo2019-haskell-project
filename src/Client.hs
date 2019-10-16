
module Client where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (recv)
import Control.Monad.IO.Class (liftIO)

import ByteStringParser (decodePlayer, decodeCards)
import FieldModifications (assignFields)
import Graphics (startGameDraw)
import Types (ClientState(..), TurnState(..), messageSize)

main :: IO ()
main = client 5005

client :: PortNumber -> IO ()
client port = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just $ show port)
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  clSt <- startGame sock
  startGameDraw clSt


startGame :: Socket -> IO ClientState
startGame sock = do
  liftIO $ print "game started"
  p <- recv sock messageSize
  let player = decodePlayer p
  let fields = assignFields
  c <- recv sock messageSize
  let cards = decodeCards c
  return $ ClientState player sock fields cards EmptyState
