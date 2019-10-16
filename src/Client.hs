
module Client where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import Control.Monad.Reader (ReaderT, runReaderT, ask, local, liftIO)
import Control.Monad.IO.Class (liftIO)

import ByteStringParser (decodePlayer, decodeCards)
import FieldModifications (assignFields)
import Graphics (startGameDraw)
import Types (ClientState(..), TurnState(..), messageSize)

client :: PortNumber -> IO ()
client port = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just $ show port)
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  clSt <- startGame sock
  startGameDraw clSt


startGame :: Socket -> IO ClientState
startGame socket = do
  liftIO $ print "game started"
  p <- recv socket messageSize
  let player = decodePlayer p
  let fields = assignFields
  c <- recv socket messageSize
  let cards = decodeCards c
  return $ ClientState player socket fields cards EmptyState
