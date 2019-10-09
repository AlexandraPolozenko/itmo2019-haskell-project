module Server where


import Network.Socket.ByteString
import Control.Concurrent (forkIO)
import qualified Data.ByteString.Char8 as B8
import System.Environment (getArgs)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)
-- import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, local)
import System.Random.Shuffle
import System.Random
import Types
import FieldModifications
import ByteStringParser


server :: PortNumber -> IO ()
server port = withSocketsDo $ do
                sock <- socket AF_INET Stream defaultProtocol
                bind sock (SockAddrInet port 0)
                listen sock 2
                sockHandler sock                 
                close sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
-- Принимаем входящее подключение.
  (sock1, _) <- accept sock
  (sock2, _) <- accept sock
  id <- getPlayerIds
  if id == 1
  then do
    _ <- forkIO $ (send sock1 (encode One)) >> putStrLn "3"
    _ <- forkIO $ (send sock2 (encode Two)) >> putStrLn "4"
    startGame sock1 sock2
  else do
    _ <- forkIO $ (send sock1 (B8.pack "2")) >> putStrLn "3"
    _ <- forkIO $ (send sock2 (B8.pack "1")) >> putStrLn "4"
    startGame sock2 sock1
  putStrLn "blabla"

receiveMessage :: Socket -> IO ()
receiveMessage sockh = do
  msg <- recv sockh 10 -- Получаем только 10 байт.
  B8.putStrLn msg -- Выводим их.
-- Если сообщение было пусто или оно равно "q" (quit)
  if msg == B8.pack "q" || B8.null msg
-- Закрываем соединение с клиентом.
  then close sockh >> putStrLn "Client disconnected"
  else do
    send sockh (B8.reverse msg)
    receiveMessage sockh -- Или получаем следующее сообщение.

startGame :: Socket -> Socket -> IO World
startGame s1 s2 = do
  let fields = assignFields
  cards <- assignCards
  let hand1 = take 7 cards
  let hand2 = take 7 (drop 7 cards)
  let cards' = drop 14 cards
  return (World fields cards')  


-- startGame :: Socket -> IO ()
-- startGame s1 = do
--   let world = assignWorld
--   i <- send s1 (B8.pack (show world))
--   putStrLn "send world"

-- assignWorld :: World
-- assignWorld = World assignFields assignCards

assignFields :: [Field]
assignFields = [Field (Combination []) (Combination []) Open | i <- [1 .. 9]]

assignCards :: IO [Card]
assignCards = 
  let cards1 = [Card Red i | i <- [1 .. 10]]	  
      cards2 = [Card Blue i | i <- [1 .. 10]]
      cards3 = [Card Yellow i | i <- [1 .. 10]] 
      cards4 = [Card Green i | i <- [1 .. 10]] 
      cards5 = [Card White i | i <- [1 .. 10]] 
      cards6 = [Card Black i  | i <- [1 .. 10]]
      cards = cards1 ++ cards2 ++ cards3 ++ cards4 ++ cards5 ++ cards6
  in
    do 
      g <- getStdGen
      return (shuffle' cards (length cards) g)


getPlayerIds :: IO Int
getPlayerIds = do
  g <- getStdGen
  let res = shuffle' [1, 2] 2 g
  return (res !! 0)

