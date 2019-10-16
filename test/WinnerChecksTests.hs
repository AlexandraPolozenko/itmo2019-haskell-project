module WinnerChecksTests (comboTests, fightTests, checkWinnerTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import FieldModifications (combo, fight, checkWinner)
import Types (Field(..), Card(..), Suit(..), Player(..), FieldState(..), Combo(..))


comboTests :: TestTree
comboTests = testGroup "tests for comdo function"
  [
    testCase "not enought cards for combo" $
      combo [(Card Yellow 3), (Card Blue 5)] @?= Nothing

  , testCase "phalanx" $
      combo [(Card Yellow 5), (Card Blue 5), (Card Red 5)] @?= Just (Phalanx 5)

  , testCase "wedge" $
      combo [(Card Yellow 5), (Card Yellow 6), (Card Yellow 7)] @?= Just (Wedge 7)

  , testCase "batalion" $
      combo [(Card Yellow 5), (Card Yellow 7), (Card Yellow 9)] @?= Just (Batallion 21)
   
  , testCase "skirmish" $
      combo [(Card Yellow 5), (Card Blue 6), (Card Green 7)] @?= Just (Skirmish 7)

  , testCase "host" $
      combo [(Card Yellow 5), (Card Blue 7), (Card Green 9)] @?= Just (Host 21)
  ]

fightTests :: TestTree
fightTests = testGroup "tests for fight function"
  [
    testCase "1" $
      fight Nothing (Just (Host 8)) @?= Open

  , testCase "2" $
      fight (Just (Host 11)) (Just (Host 8)) @?= (Closed One)
   
  , testCase "3" $
      fight (Just (Wedge 9)) (Just (Host 20)) @?= (Closed One)
  
  , testCase "4" $
      fight (Just (Batallion 21)) (Just (Host 21)) @?= (Closed One)
  ]

checkWinnerTests :: TestTree
checkWinnerTests = testGroup "tests for checkWinner function"
  [
    testCase "no winner" $
      checkWinner [Field [] [] Open, Field [] [] Open, Field [] [] Open, Field [] [] Open] @?= Nothing

  , testCase "three close" $
      checkWinner [Field [] [] (Closed One), Field [] [] (Closed One), Field [] [] (Closed One), Field [] [] Open] @?= (Just One)
   
  , testCase "five random" $
      checkWinner [Field [] [] (Closed One), Field [] [] Open, Field [] [] (Closed One), Field [] [] Open, Field [] [] (Closed One), Field [] [] Open, Field [] [] (Closed One), Field [] [] (Closed One)] @?= (Just One)
  ]