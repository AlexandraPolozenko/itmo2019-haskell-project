module GameEvents (handler) where

import Graphics.Gloss.Interface.IO.Game (Event(..),  SpecialKey(..), Key(..), KeyState(..))
import ByteStringParser (decodeCommand, decodeChanges, encode)
import qualified Data.ByteString.Char8 as B8 (pack)
import Control.Monad.IO.Class (liftIO)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import System.Exit (exitSuccess)
import Control.Concurrent (forkIO)

import FieldModifications (putCard, closeField)
import Types (
    ClientState(..)
  , Field(..)
  , Player(..)
  , Changes(..)
  , StateChanges(..)
  , FieldState(..)
  , Card(..)
  , TurnState(..)
  , Command(..)
  , Suit(..)
  , Turn(..)
  , messageSize
  , defaultCard
  )


handler :: Event -> ClientState -> IO ClientState
handler (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
handler (EventKey (SpecialKey KeySpace) Down _ _) st@(ClientState pl socket fields cards EmptyState) = do
  liftIO $ print "entered critical block"
  _ <- forkIO $ send socket (B8.pack "ready") >> print "ready"
  ch <- liftIO $ recv socket messageSize
  liftIO $ print "recieved chnges"
  let (Changes changes) = decodeChanges ch
  liftIO $ print $ show changes
  let (newSt@(ClientState p sock flds crds _), winner) = proceedChanges st changes
  case winner of
    Nothing -> do
      cmd <- liftIO $ recv socket messageSize
      liftIO $ print $ "recieved command " ++ (show cmd)
      case (decodeCommand cmd) of
        Put -> return (ClientState p sock flds crds (PutCardTurn Nothing))
        Proof -> return (ClientState pl sock flds crds (MakeProofTurn Nothing []))
        _ -> return newSt
    (Just a) -> return (ClientState p sock flds crds (GameFinished a))
handler (EventKey (Char c) Down _ _) st@(ClientState p socket fields cards (PutCardTurn Nothing)) = do
  let n = getCardNum c 
  if (length cards) <= n
  then return st
  else return (ClientState p socket fields cards (PutCardTurn (Just (cards !! n))))
handler (EventKey (Char c) Down _ _) st@(ClientState p socket fields cards (PutCardTurn (Just card))) = do
  let n = getFieldNum c 
  if notCorrectField n fields
  then return st
  else do
    _ <- forkIO $ send socket (encode (PutCard n p card)) >> print "sent card"
    return (ClientState p socket (putCard fields n p card) (filter (\e -> e /= card) cards) EmptyState)
handler (EventKey (Char 'q') Down _ _) st@(ClientState p socket fields cards (MakeProofTurn _ _)) = do
  _ <- forkIO $ send socket (encode (MakeProof 10 p [])) >> print "sent card"
  com <- liftIO $ recv socket messageSize
  let cmd = decodeCommand com
  case cmd of
    (Take crd) -> return (ClientState p socket fields (cards ++ [crd]) EmptyState)
    _ -> return st
handler (EventKey (Char c) Down _ _) st@(ClientState p socket fields cards (MakeProofTurn Nothing a)) = do
  let n = getFieldNum c 
  if notCorrectField n fields
  then return st
  else return (ClientState p socket fields cards (MakeProofTurn (Just n) a))
handler (EventKey (Char c) Down _ _) st@(ClientState p socket fields cards (MakeProofTurn (Just n) _)) = do
  _ <- forkIO $ send socket (encode (MakeProof n p [Card Black 0])) >> print "sent card"
  com <- liftIO $ recv socket messageSize
  let cmd = decodeCommand com
  case cmd of
    (Take crd) -> 
      if (crd == defaultCard)
      then return (ClientState p socket (closeField fields n p) cards EmptyState)
      else return (ClientState p socket (closeField fields n p) (cards ++ [crd]) EmptyState)
    _ -> return st
handler _ w = return w

notCorrectField :: Int -> [Field] -> Bool
notCorrectField n fields = 
  if n > 9
  then True
  else
    let (Field _ _ state) = fields !! n
    in case state of 
      Open -> False
      _ -> True

getFieldNum :: Char -> Int
getFieldNum 'a' = 1
getFieldNum 's' = 2
getFieldNum 'd' = 3
getFieldNum 'f' = 4
getFieldNum 'g' = 5
getFieldNum 'h' = 6
getFieldNum 'j' = 7
getFieldNum 'k' = 8
getFieldNum 'l' = 9
getFieldNum _ = 10

getCardNum :: Char -> Int
getCardNum 'z' = 0
getCardNum 'x' = 1
getCardNum 'c' = 2
getCardNum 'v' = 3
getCardNum 'b' = 4
getCardNum 'n' = 5
getCardNum 'm' = 6
getCardNum _ = 7


proceedChanges :: ClientState -> [StateChanges] -> (ClientState, Maybe Player)
proceedChanges st ((Winner p):_) = (st, (Just p))
proceedChanges (ClientState pl socket fields cards state) ((NewCard f p c):xs) = 
  proceedChanges (ClientState pl socket (putCard fields f p c) cards state) xs
proceedChanges (ClientState pl socket fields cards state) ((FieldClosed f p):xs) = 
  proceedChanges (ClientState pl socket (closeField fields f p) cards state) xs
proceedChanges st [] = (st, Nothing)