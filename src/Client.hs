
module Client where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8
import Types
import ByteStringParser
import FieldModifications
import Graphics
import Control.Monad.Reader (ReaderT, runReaderT, ask, local, liftIO)
import Control.Monad.IO.Class (liftIO)


portNumber = 5005 :: Int

main :: IO ()
main = do
  client "127.0.0.1" 5005


client :: String -> Int -> IO ()
client host port = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  clSt <- startGame sock
  startGameDraw clSt
  print clSt
  -- runReaderT (turn sock) clSt
  -- msgSender sock

-- msgSender :: Socket -> IO ()
-- msgSender sock = do
--   msg <- B8.getLine
--   send sock msg
--   rMsg <- recv sock 10000
--   B8.putStrLn rMsg
--   if msg == B8.pack "q" then putStrLn "Disconnected!" else msgSender sock


startGame :: Socket -> IO ClientState
startGame s = do
  pp <- recv s messageSize
  let p = decodePlayer pp
  let f = assignFields
  cc <- recv s messageSize
  let c = decodeCards cc
  return $ ClientState p s f c EmptyState

-- turn :: Socket -> ReaderT ClientState IO ()
-- turn = phasePut

-- phasePut :: Socket -> ReaderT ClientState IO ()
-- phasePut s = do
--   cmd <- liftIO $ recv s messageSize
--   (ClientState p f cs) <- ask
--   case decodeCommand cmd of
--     Put -> do
--       g <- liftIO getStdGen
--       let card = head (shuffle' cs 7 g)
--       liftIO $ send s (encode (PutCard 1 p card))
--       liftIO $ print card
--       turn s
--     Win p -> undefined
--     _ -> error ""