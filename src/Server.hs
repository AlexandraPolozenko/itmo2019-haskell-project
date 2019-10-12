module Server where


import Network.Socket.ByteString
import Control.Concurrent (forkIO)
import qualified Data.ByteString.Char8 as B8
import System.Environment (getArgs)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)
import System.Random.Shuffle
import System.Random
import Types
import FieldModifications
import ByteStringParser
-- import Control.Monad.Reader (ReaderT, runReaderT, ask, local, liftIO)

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
  id <- getPlayerIds
  if id == 1
  then do
    _ <- forkIO $ (send sock1 (encode One)) >> putStrLn "3"
    _ <- forkIO $ (send sock2 (encode Two)) >> putStrLn "4"
    startGame sock1 sock2
  else do
    _ <- forkIO $ (send sock1 (encode Two)) >> putStrLn "3"
    _ <- forkIO $ (send sock2 (encode One)) >> putStrLn "4"
    startGame sock2 sock1

  -- runGame sock1 sock2
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

-- runGame :: Socket -> Socket -> ReaderT World IO ()
-- runGame s1 s2 = undefined
--   -- send s1 (encode MakeTurn)
--   -- t1 <- recv s1 messageSize
--   -- let turn1 = decodeTurn t1 
--   -- makeTurn turn1
--   -- ctx <- ask
--   -- let valid = validateTurn turn1 ctx
--   -- makeTurn turn1
--   -- send 


-- interactTurn :: Socket -> ReaderT World IO ()
-- interactTurn s = undefined
	-- t <- recv s messageSize
	-- let turn = decodeTurn t
	-- case turn of FinishTurn -> return ()
	-- 	         otherwise -> turn
	-- world <- changeWorld ask turn



-- changeWorld :: World -> Turn -> World
-- changeWorld (World fields cards) (PutCard id player card) = 
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



getPlayerIds :: IO Int
getPlayerIds = do
  g <- getStdGen
  let res = shuffle' [1, 2] 2 g
  return (res !! 0)

