module Server where

import Control.Concurrent (forkIO)
import System.Environment (getArgs)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)
import System.Random.Shuffle (shuffle')
import System.Random (getStdGen)
import Control.Monad.Reader (ReaderT, runReaderT, ask, local, liftIO)
import Control.Monad.IO.Class (liftIO)

import FieldModifications
import ByteStringParser
import Types (
    World(..)
  , Changes(..)
  , Command(..)
  , Turn(..)
  , Player(..)
  , Field(..)
  , FieldState(..)
  , StateChanges(..)
  , messageSize
  , defaultCard
  )


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
  _ <- forkIO $ send player1 (encode One) >> putStrLn "Player One"
  _ <- forkIO $ send player2 (encode Two) >> putStrLn "Player Two"
  world <- startGame player1 player2
  runReaderT (turn player1 player2) world
  putStrLn "game finished"

startGame :: Socket -> Socket -> IO World
startGame s1 s2 = do
  let fields = assignFields
  cards <- assignCards
  let hand1 = take 7 cards
  _ <- forkIO $ (send s1 (encode hand1)) >> putStrLn "send 1"
  let hand2 = take 7 (drop 7 cards)
  _ <- forkIO $ (send s2 (encode hand2)) >> putStrLn "send 2"
  let cards' = drop 14 cards
  return (World fields cards' [])

turn :: Socket -> Socket -> ReaderT World IO ()
turn = phasePut

phasePut :: Socket -> Socket -> ReaderT World IO ()
phasePut p1 p2 = do
  (World fields _ changes) <- ask
  liftIO $ print "waiting for client to be ready"
  ok <- liftIO $ recv p1 messageSize
  liftIO $ print $ show ok
  let pl = checkWinner fields
  case pl of
    (Just a) -> do
      liftIO $ print $ "found winner" ++ show a
      liftIO $ send p1 (encode (Changes (changes ++ [Winner a])))
      _ <- liftIO $ recv p2 messageSize
      liftIO $ send p2 (encode (Changes (changes ++ [Winner a])))
      return ()
    Nothing -> do
      liftIO $ send p1 (encode (Changes changes))
      liftIO $ print $ "going to send"
      liftIO $ send p1 (encode Put)
      liftIO $ print "waiting for message"
      t <- liftIO $ recv p1 messageSize
      liftIO $ print $ "recieved" ++ (show t)
      case decodeTurn t of
        PutCard fNum p c -> do
          liftIO $ print c
          local (\ (World fs cs _) -> World (putCard fs fNum p c) cs [NewCard fNum p c]) (phaseProof p1 p2)
        _ -> error ""


phaseProof :: Socket -> Socket -> ReaderT World IO ()
phaseProof p1 p2 = do
  (World fields cards _) <- ask
  liftIO $ print "turn"
  ok <- liftIO $ recv p1 messageSize
  liftIO $ print $ show ok
  liftIO $ send p1 (encode (Changes []))
  liftIO $ print $ "going to send"
  liftIO $ send p1 (encode Proof)
  liftIO $ print "waiting for message"
  t <- liftIO $ recv p1 messageSize
  liftIO $ print $ "recieved" ++ (show t)
  let (card, left) = if (length cards) == 0 then (defaultCard, []) else ((head cards), (tail cards))
  case decodeTurn t of
    (MakeProof 10 _ _) -> do
      liftIO $ send p1 (encode (Take card))
      local (\(World f c chng) -> World f left chng) (turn p2 p1)
    (MakeProof n p _) -> do
      liftIO $ send p1 (encode (Take card))
      local (\(World f c chng) -> World (closeField f n p) left (chng ++ [FieldClosed n p])) (turn p2 p1)

checkWinner :: [Field] -> Maybe Player
checkWinner fields = 
  case (threeFieldsNear fields 0 0) of
    Nothing -> fiveFields fields 0 0
    (Just a) -> (Just a)


threeFieldsNear :: [Field] -> Int -> Int -> Maybe Player
threeFieldsNear [] b1 b2 =
  if b1 == 3
  then (Just One)
  else 
    if b2 == 3
    then (Just Two)
    else Nothing
threeFieldsNear ((Field _ _ st):fields) b1 b2 = 
  if b1 == 3
  then (Just One)
  else
    if b2 == 3
    then (Just Two)
    else
      case st of
        Open -> threeFieldsNear fields 0 0
        (Types.Closed One) -> threeFieldsNear fields (b1 + 1) 0
        (Types.Closed Two) -> threeFieldsNear fields 0 (b2 + 1)


fiveFields :: [Field] -> Int -> Int -> Maybe Player
fiveFields [] b1 b2 =
  if b1 == 5
  then (Just One)
  else 
    if b2 == 5
    then (Just Two)
    else Nothing
fiveFields ((Field _ _ st):fields) b1 b2 = 
  if b1 == 5
  then (Just One)
  else
    if b2 == 5
    then (Just Two)
    else
      case st of
        Open -> threeFieldsNear fields b1 b2
        (Types.Closed One) -> threeFieldsNear fields (b1 + 1) b2
        (Types.Closed Two) -> threeFieldsNear fields b1 (b2 + 1)


setPlayers :: [Socket] -> IO [Socket]
setPlayers players = do
  g <- getStdGen
  return $ shuffle' players 2 g

