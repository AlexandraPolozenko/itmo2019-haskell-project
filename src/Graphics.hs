module Graphics(startGameDraw) where

import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss (
    Picture(..)
  , Display(..)
  , pictures
  , translate
  , color
  , rectangleWire
  , rectangleSolid
  , text
  , scale
  , rotate
  , red
  , white
  , black
  , blue
  , yellow
  , green
  , greyN
  )

import Types (ClientState(..), TurnState(..), Field(..), FieldState(..), Player(..), Suit(..), Card(..))
import GameEvents (handler)


startGameDraw :: ClientState -> IO ()
startGameDraw initState = playIO (InWindow "myWindow" (600, 350) (0, 0)) (greyN 0.25) 30 initState drawPicture handler updater

updater _ w = return w

drawPicture :: ClientState -> IO Picture
drawPicture (ClientState p _ fields cards st) = return $ pictures [(drawField 0 fields), (drawHand (-570) cards), drawState st, drawPlayer p]

drawPlayer :: Player -> Picture
drawPlayer One = pictures [translate (-100) (-250) $ scale 0.1 0.1 $ color white $ text "Player 1"]
drawPlayer Two = pictures [translate (-100) (-250) $ scale 0.1 0.1 $ color white $ text "Player 2"]

drawState :: TurnState -> Picture
drawState (GameFinished p) = pictures [translate (-100) (-50) $ color red $ rectangleSolid 600 400
                                      , translate (-100) (-25) $ scale 0.3 0.3 $ color black $ text $ "Player " ++ (show p) ++ " win"]
drawState EmptyState = pictures [translate (-100) (-300) $ scale 0.2 0.2 $ color white $ text "press space to refresh game state"]
drawState (PutCardTurn Nothing) = pictures [translate (-100) (-300) $ scale 0.2 0.2 $ color white $ text "choose card (z - m)"]
drawState (PutCardTurn (Just a)) = pictures [translate (-100) (-300) $ scale 0.2 0.2 $ color white $ text "choose field (a - l)"]
-- drawState (MakeProofTurn Nothing _) = pictures [translate (-100) (-300) $ scale 0.2 0.2 $ color white $ text "choose field (a - l), or press q to skip phase"]
-- drawState (MakeProofTurn (Just a) _) = pictures [translate (-100) (-300) $ scale 0.2 0.2 $ color white $ text "choose card (field1: q-z, ...), or press enter to finish"]

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
drawClosed w One = pictures [translate (w) (-60) $ color green $ rectangleWire 115 265]
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
drawHand k (x:xs) = pictures [translate k (-300) $ drawCard x, drawHand (k + 50) xs]

drawCard :: Card -> Picture
drawCard (Card Red v) = pictures ([color red $ rectangleSolid 100 150, color black $ rectangleWire 100 150] ++ drawNumber v Red)
drawCard (Card Yellow v) = pictures ([color yellow $ rectangleSolid 100 150, color black $ rectangleWire 100 150] ++ drawNumber v Yellow)
drawCard (Card Blue v) = pictures ([color blue $ rectangleSolid 100 150, color black $ rectangleWire 100 150] ++ drawNumber v Blue)
drawCard (Card Green v) = pictures ([color green $ rectangleSolid 100 150, color black $ rectangleWire 100 150] ++ drawNumber v Green)
drawCard (Card White v) = pictures ([color white $ rectangleSolid 100 150, color black $ rectangleWire 100 150] ++ drawNumber v White)
drawCard (Card Black v) = pictures ([color black $ rectangleSolid 100 150, color white $ rectangleWire 100 150] ++ drawNumber v Black)


drawNumber :: Int -> Suit -> [Picture]
drawNumber v Black = [translate (-45) 35 $ scale 0.3 0.3 $ color white $ text (show v)
                     , rotate 180 $ translate (-45) 35 $ scale 0.3 0.3 $ color white $ text (show v)
                     ]
drawNumber v _ = [translate (-45) 35 $ scale 0.3 0.3 $ color black $ text (show v)
                , rotate 180 $ translate (-45) 35 $ scale 0.3 0.3 $ color black $ text (show v)
                ]





