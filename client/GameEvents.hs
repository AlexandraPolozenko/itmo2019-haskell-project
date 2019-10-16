module GameEvents (handler) where

import Graphics.Gloss.Interface.IO.Game (Event(..),  SpecialKey(..), Key(..), KeyState(..))
import Control.Monad.IO.Class (liftIO)
import Network.Socket.ByteString (send, recv)
import System.Exit (exitSuccess)
import Control.Concurrent (forkIO)
import System.Timeout (timeout)

import ByteStringParser (decodeCommand, decodeChanges, encode)
import FieldModifications (putCard, closeField)
import Types (
    ClientState(..)
  , Field(..)
  , Player(..)
  , Changes(..)
  , StateChanges(..)
  , FieldState(..)
  , TurnState(..)
  , Command(..)
  , Turn(..)
  , messageSize
  , defaultCard
  )

timeoutMicroseconds :: Int
timeoutMicroseconds = 10000000

handler :: Event -> ClientState -> IO ClientState
handler (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
handler (EventKey (SpecialKey KeySpace) Down _ _) st@(ClientState _ sock _ _ EmptyState) = do
  ch <- timeout timeoutMicroseconds (recv sock messageSize)
  liftIO $ print "recieved chnges"
  let (Changes changes) = decodeChanges ch
  liftIO $ print $ show changes
  let ((ClientState p _ flds crds _), winner) = proceedChanges st changes
  case winner of
    Nothing -> do
      return (ClientState p sock flds crds (PutCardTurn Nothing))
    (Just a) -> return (ClientState p sock flds crds (GameFinished a))
handler (EventKey (Char c) Down _ _) st@(ClientState p sock fields cards (PutCardTurn Nothing)) = do
  let n = getCardNum c
  if (length cards) <= n
  then return st
  else return (ClientState p sock fields cards (PutCardTurn (Just (cards !! n))))
handler (EventKey (Char c) Down _ _) st@(ClientState p sock fields cards (PutCardTurn (Just card))) = do
  let n = getFieldNum c
  if notCorrectField n fields p
  then return st
  else do
    _ <- forkIO $ send sock (encode (PutCard n p card)) >> print "sent card"
    com <- timeout timeoutMicroseconds (recv sock messageSize)
    let cmd = decodeCommand com
    liftIO $ print "take card"
    case cmd of
      (Take crd) ->
        if (crd == defaultCard)
        then return (ClientState p sock (putCard fields n p card) (filter (\e -> e /= card) cards) EmptyState)
        else return (ClientState p sock (putCard fields n p card) ((filter (\e -> e /= card) cards) ++ [crd]) EmptyState)
      _ -> return (ClientState p sock (putCard fields n p card) (filter (\e -> e /= card) cards) EmptyState)
handler _ w = return w

notCorrectField :: Int -> [Field] -> Player -> Bool
notCorrectField n fields p =
  if n > 9
  then True
  else
    let f@(Field _ _ state) = fields !! (n - 1)
    in case state of
      Open -> checkSpace f p
      _ -> True

checkSpace :: Field -> Player -> Bool
checkSpace (Field comb1 _ _) One =
  if (length comb1) == 3
  then True
  else False
checkSpace (Field _ comb2 _) Two =
  if (length comb2) == 3
  then True
  else False


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
proceedChanges (ClientState pl sock fields cards state) ((NewCard f p c):xs) =
  proceedChanges (ClientState pl sock (putCard fields f p c) cards state) xs
proceedChanges (ClientState pl sock fields cards state) ((FieldClosed f p):xs) =
  proceedChanges (ClientState pl sock (closeField fields f p) cards state) xs
proceedChanges st [] = (st, Nothing)
