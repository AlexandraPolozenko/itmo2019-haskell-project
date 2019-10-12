module FieldModifications where

import Types
import System.Random.Shuffle
import System.Random

assignFields :: [Field]
assignFields = [Field [] [] Open | i <- [1 .. 9]]

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


