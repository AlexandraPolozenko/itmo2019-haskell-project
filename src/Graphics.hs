module Graphics where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Control.Concurrent
import Types

startGameDraw :: ClientState -> IO ()
startGameDraw initState = play FullScreen (greyN 0.25) 30 initState renderer handler updater

renderer _ = pictures [translate 0 1 $ color red $ rectangleSolid 20 30, translate 2 2 $ color yellow $ rectangleSolid 20 30]

updater _ = id
handler _ = id

-- drawPicture :: ClientState -> Picture
-- drawPicture (ClientState _ fields cards) = pictures $ (drawField fields) ++ (drawCards cards)

-- drawField :: [Field] -> Picture
-- drawField fields = picture [drawOneField f | f <- fields]

-- drawOneField :: Field -> Picture
-- drawOneField (Field comb1 comb2 state) = picture [drawCombination comb1 1, drawCombination comb2 2]


-- Red | Blue | Yellow | Green | White | Black


-- drawCard :: Card -> Picture
-- drawCard (Card Red v) = pictures [color red $ rectangleSolid 20 30
--                                     , scale 0.15 0.15 $ color black $ text (show v)
--                                     ]

