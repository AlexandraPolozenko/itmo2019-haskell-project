module FieldModifications(assignFields, assignCards, putCard, closeField) where

import Types (Field(..), FieldState(..), Card(..), Player(..), Suit(..))
import System.Random.Shuffle (shuffle')
import System.Random (getStdGen)

assignFields :: [Field]
assignFields = replicate 9 (Field [] [] Open)

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


putCard :: [Field] -> Int -> Player -> Card -> [Field]
putCard fields ind p card =
  let left = take (ind - 1) fields
      right = drop ind fields
      (Field comb1 comb2 state) = fields !! (ind - 1)
  in
    case state of 
      (Closed _) -> fields
      _ -> case p of
        One -> 
          if (length comb1) == 3
          then fields
          else left ++ [Field (comb1 ++ [card]) comb2 state] ++ right
        Two ->
          if (length comb2) == 3
          then fields
          else left ++ [Field comb1 (comb2 ++ [card]) state] ++ right


closeField :: [Field] -> Int -> Player -> [Field]
closeField fields ind p = 
  let left = take (ind - 1) fields
      right = drop ind fields
      (Field comb1 comb2 _) = fields !! (ind - 1)
  in left ++ [Field comb1 comb2 (Closed p)] ++ right