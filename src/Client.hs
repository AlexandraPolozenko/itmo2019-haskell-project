module Client where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8
import Types



main :: IO ()
main = do
  client "127.0.0.1" 5005

client :: String -> Int -> IO ()
client host port = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  msgSender sock
  close sock

msgSender :: Socket -> IO ()
msgSender sock = do
  msg <- B8.getLine
  send sock msg
  rMsg <- recv sock 10000
  B8.putStrLn rMsg
  if msg == B8.pack "q" then putStrLn "Disconnected!" else msgSender sock


