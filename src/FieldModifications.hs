module FieldModifications(
  assignFields
  , assignCards
  , putCard
  , closeField
  , checkField
  , checkWinner
  , combo
  , fight
  ) where

import Types (Field(..), FieldState(..), Card(..), Player(..), Suit(..), Combo(..))
import System.Random.Shuffle (shuffle')
import System.Random (getStdGen)
import Data.List (sortBy)


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