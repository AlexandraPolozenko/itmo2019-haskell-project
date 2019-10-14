module Graphics where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import System.Exit
import Control.Concurrent
import Types
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8
import Types
import ByteStringParser
import FieldModifications
import Control.Monad.Reader (ReaderT, runReaderT, ask, local, liftIO)
import Control.Monad.IO.Class (liftIO)

startGameDraw :: ClientState -> IO ()
startGameDraw initState = playIO (InWindow "myWindow" (600, 350) (0, 0)) (greyN 0.25) 30 initState drawPicture handler updater

-- renderer _ = pictures [drawCard (Card Red 5), translate (-570) (-300) $ drawCard (Card Yellow 3), translate (-520) (-300) $ drawCard (Card Blue 10), translate 50 50 $ drawCard (Card Black 9)]  --pictures [translate 0 0 $ color red $ rectangleSolid 100 150, translate 300 300 $ color yellow $ rectangleSolid 100 150]
messageSize = 10000 :: Int

updater _ w = return w

handler :: Event -> ClientState -> IO ClientState
handler (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
handler (EventKey (SpecialKey KeySpace) Down _ _) st@(ClientState pl socket fields cards EmptyState) = do
  _ <- forkIO $ send socket (B8.pack "ready") >> print "ready"
  ch <- liftIO $ recv socket messageSize
  liftIO $ print "recieved chnges"
  let changes = decodeChanges ch
  liftIO $ print $ show changes
  let (newSt@(ClientState p sock flds crds _), winner) = proceedChanges st changes
  case winner of
    Nothing -> do
      cmd <- liftIO $ recv socket messageSize
      liftIO $ print $ "recieved command " ++ (show cmd)
      case (decodeCommand cmd) of
        Put -> return (ClientState p sock flds crds (PutCardTurn Nothing))
        Proof -> return (ClientState pl sock flds crds (MakeProofTurn Nothing []))
        _ -> return st
    _ -> return st --FINNISH!!!!!!!
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
handler (EventKey (Char c) Down _ _) st@(ClientState p socket fields cards (MakeProofTurn Nothing a)) = do
  let n = getFieldNum c 
  if notCorrectField n fields
  then return st
  else return (ClientState p socket fields cards (MakeProofTurn (Just n) a))
handler (EventKey (Char c) Down _ _) st@(ClientState p socket fields cards (MakeProofTurn (Just n) _)) = do
  _ <- forkIO $ send socket (encode (MakeProof n [Card Black 0])) >> print "sent card"
  com <- liftIO $ recv socket messageSize
  let cmd = decodeCommand com
  case cmd of
    (Take crd) -> return (ClientState p socket fields (cards ++ [crd]) EmptyState)
-- handler (EventKey (SpecialKey KeySpace) _ _ _) st@(ClientState pl socket fields cards (PutCardTurn a)) = return (ClientState pl socket fields [] (PutCardTurn a))
-- handler (EventKey (SpecialKey KeyRight) Down _ _) (ClientState pl socket fields cards a) = return (ClientState pl socket fields [] a)
-- handler (EventKey (SpecialKey KeyLeft) Down _ _) (ClientState pl socket fields cards a) = sendHelper (ClientState pl socket fields cards a)
handler _ w = return w


notCorrectField :: Int -> [Field] -> Bool
notCorrectField n fields = 
  if n > 8
  then True
  else
    let (Field _ _ state) = fields !! n
    in case state of 
      Open -> False
      _ -> True

getFieldNum :: Char -> Int
getFieldNum 'a' = 0
getFieldNum 's' = 1
getFieldNum 'd' = 2
getFieldNum 'f' = 3
getFieldNum 'g' = 4
getFieldNum 'h' = 5
getFieldNum 'j' = 6
getFieldNum 'k' = 7
getFieldNum 'l' = 8
getFieldNum _ = 9

getCardNum :: Char -> Int
getCardNum 'z' = 0
getCardNum 'x' = 1
getCardNum 'c' = 2
getCardNum 'v' = 3
getCardNum 'b' = 4
getCardNum 'n' = 5
getCardNum 'm' = 6
getCardNum _ = 7

-- FINISH !!!!!
proceedChanges :: ClientState -> Changes -> (ClientState, Maybe Player)
proceedChanges a b = (a, Nothing)

-- handler (EventKey (MouseButton LeftButton) Down _ mouse) st@(ClientState pl socket fields cards (PutCardTurn a)) = do
--   case a of
--     Nothing -> return (ClientState pl socket fields cards (PutCardTurn (Just (mouseToCard mouse st))))
--     (Just card) -> do
--       let field = 1
--       _ <- print card
--       _ <- send socket (encode (PutCard field pl card))
--       _ <- print "sent"
--       return (ClientState pl socket (putCard fields field pl card) cards EmptyState)

sendHelper :: ClientState -> IO ClientState
sendHelper st@(ClientState pl socket fields cards a) = do
  _ <- forkIO $ send socket (encode (PutCard 1 One (Card Red 10))) >> print "sent card"
  return (ClientState pl socket fields (cards ++ [Card Red 10]) a)


mouseToCard :: (Float, Float) -> ClientState -> Card
mouseToCard (w, h) (ClientState _ _ f cards _) = 
  if h < (-150)
  then 
    let ind =  (round ((w + 570) / 30)) 
    in
      if (ind > length cards)
      then (Card Red 3)
      else cards !! ind
  else (Card Yellow 3)
    -- let ind = (w + 490) / 120
    -- in

drawPicture :: ClientState -> IO Picture
drawPicture (ClientState _ _ fields cards st) = return $ pictures [(drawField 0 fields), (drawHand (-570) cards), translate 400 (-300) $ scale 0.3 0.3 $ color black $ text (show st)]

drawField :: Int -> [Field] -> Picture
drawField k (f:[]) = pictures [drawOneField k f]
drawField k (f:fields) = pictures [drawOneField k f, drawField (k + 1) fields]

drawOneField :: Int -> Field -> Picture
drawOneField id (Field comb1 comb2 state) = 
  let w = fromIntegral ((-490) + 120 * id) :: Float
      p = pictures [(drawCombinationDown w (-15) comb1), (drawCombinationUp w 185 comb2), (drawCombinationSeparator w 85)]
  in
    case state of 
      (Types.Closed player) -> pictures [p, drawClosed w player]
      _ -> p

drawClosed :: Float -> Player -> Picture
drawClosed w One = pictures [translate (w) (-150) $ color green $ rectangleWire 115 265]
drawClosed w Two = pictures [translate (w) 240 $ color green $ rectangleWire 115 265]

drawCombinationDown :: Float -> Float -> [Card] -> Picture
drawCombinationDown w h [] = pictures [translate w h $ color blue $ rectangleWire 100 150]
drawCombinationDown w h (x:[]) = pictures [translate w h $ drawCard x]
drawCombinationDown w h (x:xs) = pictures [translate w h $ drawCard x, drawCombinationDown w (h - 50) xs]

drawCombinationUp :: Float -> Float -> [Card] -> Picture
drawCombinationUp w h [] = pictures [translate w h $ color red $ rectangleWire 100 150]
drawCombinationUp w h (x:[]) = pictures [translate w h $ drawCard x]
drawCombinationUp w h (x:xs) = pictures [drawCombinationUp w (h + 50) xs, translate w h $ drawCard x]

drawCombinationSeparator :: Float -> Float -> Picture
drawCombinationSeparator w h = pictures [translate w h $ color black $ rectangleWire 100 20]

drawHand :: Float -> [Card] -> Picture
drawHand k [] = pictures [translate k (-300) $ color black $ rectangleWire 100 150]
drawHand k (x:[]) = pictures [translate k (-300) $ drawCard x]
drawHand k (x:xs) = pictures [translate k (-300) $ drawCard x, drawHand (k + 30) xs]


drawCard :: Card -> Picture
drawCard (Card Red v) = pictures ([color red $ rectangleSolid 100 150, color black $ rectangleWire 100 150] ++ drawNumber v Red)
drawCard (Card Yellow v) = pictures ([color yellow $ rectangleSolid 100 150, color black $ rectangleWire 100 150] ++ drawNumber v Yellow)
drawCard (Card Blue v) = pictures ([color blue $ rectangleSolid 100 150, color black $ rectangleWire 100 150] ++ drawNumber v Blue)
drawCard (Card Green v) = pictures ([color green $ rectangleSolid 100 150, color black $ rectangleWire 100 150] ++ drawNumber v Green)
drawCard (Card White v) = pictures ([color white $ rectangleSolid 100 150, color black $ rectangleWire 100 150] ++ drawNumber v White)
drawCard (Card Black v) = pictures ([color black $ rectangleSolid 100 150, color white $ rectangleWire 100 150] ++ drawNumber v Black)


drawNumber :: Int -> Suit -> [Picture]
drawNumber 10 Black = [translate (-45) 35 $ scale 0.3 0.3 $ color white $ text "10"
                      , rotate 180 $ translate (-45) 35 $ scale 0.3 0.3 $ color white $ text "10"
                      ]
drawNumber 10 _ = [translate (-45) 35 $ scale 0.3 0.3 $ color black $ text "10"
                , rotate 180 $ translate (-45) 35 $ scale 0.3 0.3 $ color black $ text "10"
                ]
drawNumber v Black = [translate (-45) 35 $ scale 0.3 0.3 $ color white $ text (show v)
                     , rotate 180 $ translate (-45) 35 $ scale 0.3 0.3 $ color white $ text (show v)
                     ]
drawNumber v _ = [translate (-45) 35 $ scale 0.3 0.3 $ color black $ text (show v)
                , rotate 180 $ translate (-45) 35 $ scale 0.3 0.3 $ color black $ text (show v)
                ]





