module Server where

import Control.Concurrent (forkIO)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)
import System.Random.Shuffle (shuffle')
import System.Random (getStdGen)
import Control.Monad.Reader (ReaderT, runReaderT, ask, local, liftIO)
import Data.List (sortBy)

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
  , Combo(..)
  , Card(..)
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
  (player1:player2:_) <- setPlayers [sock1, sock2]
  _ <- forkIO $ send player1 (encode One) >> putStrLn "Player One"
  _ <- forkIO $ send player2 (encode Two) >> putStrLn "Player Two"
  world <- startGame player1 player2
  runReaderT (phasePut player1 player2) world
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

phasePut :: Socket -> Socket -> ReaderT World IO ()
phasePut p1 p2 = do
  (World fields cards changes) <- ask
  liftIO $ print "waiting for client to be ready"
  let pl = checkWinner fields
  case pl of
    (Just a) -> do
      liftIO $ print $ "found winner" ++ show a
      _ <- liftIO $ send p1 (encode (Changes (changes ++ [Winner a])))
      _ <- liftIO $ recv p2 messageSize
      _ <- liftIO $ send p2 (encode (Changes (changes ++ [Winner a])))
      return ()
    Nothing -> do
      _ <- liftIO $ send p1 (encode (Changes changes))
      liftIO $ print $ "going to send"
      liftIO $ print "waiting for message"
      t <- liftIO $ recv p1 messageSize
      liftIO $ print $ "recieved" ++ (show t)
      case decodeTurn t of
        PutCard fNum p c -> do
          liftIO $ print c
          let flds = putCard fields fNum p c
          let chngs = (filter filterClosedFields changes) ++ [NewCard fNum p c]
          let (card, left) = if (length cards) == 0 then (defaultCard, []) else ((head cards), (tail cards))
          _ <- liftIO $ send p1 (encode (Take card))
          case checkField (flds !! (fNum - 1)) of
            Open -> local (\ (World _ _ _) -> (World flds left chngs)) (phasePut p2 p1)
            (Types.Closed player) -> local (\ (World _ _ _) -> (World flds left (chngs ++ [FieldClosed fNum player]))) (phasePut p2 p1)
        _ -> error "Incorrect data"


filterClosedFields :: StateChanges -> Bool
filterClosedFields (FieldClosed _ _) = True
filterClosedFields _ = False

checkField :: Field -> FieldState
checkField (Field _ _ closed@(Types.Closed _)) = closed
checkField (Field p1 p2 _) = fight (combo p1) (combo p2)

fight :: Maybe Combo -> Maybe Combo -> FieldState
fight Nothing _ = Open
fight _ Nothing = Open
fight (Just c1) (Just c2) =
  if c1 > c2
  then (Types.Closed One)
  else (Types.Closed Two)

combo :: [Card] -> Maybe Combo
combo cards
  | length cards < 3 = Nothing
  | valuesEqual = (Just $ Phalanx i3)
  | suitsEqual =
      if valuesRow
      then (Just (Wedge i3))
      else (Just (Batallion (i1 + i2 + i3)))
  | otherwise =
      if valuesRow
      then (Just (Skirmish i3))
      else (Just (Host (i1 + i2 + i3)))
  where
    ((Card s1 i1):(Card s2 i2):(Card s3 i3):_) =
      sortBy (\(Card _ a) (Card _ b) -> compare a b) cards
    suitsEqual = (s1 == s2) && (s2 == s3)
    valuesEqual = (i1 == i2) && (i2 == i3)
    valuesRow = (i2 == i1 + 1) && (i3 == i2 + 1)

checkWinner :: [Field] -> Maybe Player
checkWinner fields =
  case (threeFieldsNear fields 0 0) of
    Nothing -> fiveFields fields 0 0
    (Just a) -> (Just a)


threeFieldsNear :: [Field] -> Int -> Int -> Maybe Player
threeFieldsNear [] b1 b2
  | b1 == 3 = (Just One)
  | b2 == 3 = (Just Two)
  | otherwise = Nothing
threeFieldsNear ((Field _ _ st):fields) b1 b2
  | b1 == 3 = (Just One)
  | b2 == 3 = (Just Two)
  | otherwise =
    case st of
      Open -> threeFieldsNear fields 0 0
      (Types.Closed One) -> threeFieldsNear fields (b1 + 1) 0
      (Types.Closed Two) -> threeFieldsNear fields 0 (b2 + 1)


fiveFields :: [Field] -> Int -> Int -> Maybe Player
fiveFields [] b1 b2
  | b1 == 5 = (Just One)
  | b2 == 5 = (Just Two)
  | otherwise = Nothing
fiveFields ((Field _ _ st):fields) b1 b2
  | b1 == 5 = (Just One)
  | b2 == 5 = (Just Two)
  | otherwise =
    case st of
      Open -> fiveFields fields b1 b2
      (Types.Closed One) -> fiveFields fields (b1 + 1) b2
      (Types.Closed Two) -> fiveFields fields b1 (b2 + 1)


setPlayers :: [Socket] -> IO [Socket]
setPlayers players = do
  g <- getStdGen
  return $ shuffle' players 2 g
