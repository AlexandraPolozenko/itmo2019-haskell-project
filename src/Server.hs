module Server where


import Network.Socket.ByteString
import Control.Concurrent (forkIO)
import qualified Data.ByteString.Char8 as B8
import System.Environment (getArgs)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)
import System.Random.Shuffle (shuffle')
import System.Random
import Types
import FieldModifications
import ByteStringParser
import Control.Monad.Reader (ReaderT, runReaderT, ask, local, liftIO)
import Control.Monad.IO.Class (liftIO)

server :: PortNumber -> IO ()
server port = withSocketsDo $ do
                sock <- socket AF_INET Stream defaultProtocol
                bind sock (SockAddrInet port 0)
                listen sock 2
                sockHandler sock
                close sock


sockHandler :: Socket -> IO ()
sockHandler sock = do
  (sock1, _) <- accept sock
  (sock2, _) <- accept sock
  (player1:player2:empty) <- setPlayers [sock1, sock2]
  _ <- forkIO $ send player1 (encode One) >> putStrLn "3"
  _ <- forkIO $ send player2 (encode Two) >> putStrLn "4"
  world <- startGame player1 player2
  runReaderT (turn player1 player2) world
  putStrLn "blabla"

-- receiveMessage :: Socket -> IO ()
-- receiveMessage sockh = do
--   msg <- recv sockh 10
--   B8.putStrLn msg
--   if msg == B8.pack "q" || B8.null msg
--   then close sockh >> putStrLn "Client disconnected"
--   else do
--     send sockh (B8.reverse msg)
--     receiveMessage sockh

messageSize :: Int
messageSize = 1000

startGame :: Socket -> Socket -> IO World
startGame s1 s2 = do
  let fields = assignFields
  cards <- assignCards
  let hand1 = take 7 cards
  _ <- forkIO $ (send s1 (encode hand1)) >> putStrLn "send 1"
  let hand2 = take 7 (drop 7 cards)
  _ <- forkIO $ (send s2 (encode hand2)) >> putStrLn "send 2"
  let cards' = drop 14 cards
  return (World fields cards')

turn :: Socket -> Socket -> ReaderT World IO ()
turn = phasePut

phasePut :: Socket -> Socket -> ReaderT World IO ()
phasePut p1 p2 = do
  liftIO $ print "waiting for client to be ready"
  ok <- liftIO $ recv p1 messageSize
  liftIO $ print $ show ok
  liftIO $ send p1 (encode (Changes []))
  liftIO $ print $ "going to send"
  liftIO $ send p1 (encode Put)
  liftIO $ print "waiting for message"
  t <- liftIO $ recv p1 messageSize
  liftIO $ print $ "recieved" ++ (show t)
  case decodeTurn t of
    PutCard fNum p c -> do
        liftIO $ print c
        local (\ (World fs cs) -> World (putCard fs fNum p c) cs) (phaseProof p1 p2)
    _ -> error ""


phaseProof :: Socket -> Socket -> ReaderT World IO ()
phaseProof p1 p2 = do
  w <- ask
  liftIO $ print "turn"
  -- liftIO getLine
  turn p2 p1



-- changeWorld :: World -> Turn -> World
-- changeWorld (World fields cards) (PutputStrLn "blabla"Card id player card) =
--   let f = fields


-- changeField :: [Field] -> Int -> Card -> Player -> [Field] -> [Field]
-- changeField ((Field c1 c2 st):xs) 0 c p res =
--   if st of Open
--   then
--     case p of
--       One -> res ++ [(Field (c1 ++ [c]) c2 st)] ++ xs
--       _ -> res ++ [(Field c1 (c2 ++ [c]) st)] ++ xs
-- changeField (x:xs) id c p res = changeField xs (id - 1) c p (res ++ [x])



-- validateTurn :: Turn -> World -> TurnState
-- validateTurn (Proof c) w = undefined



setPlayers :: [Socket] -> IO [Socket]
setPlayers players = do
  g <- getStdGen
  return $ shuffle' players 2 g

