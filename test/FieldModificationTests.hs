module FieldModificationTests (putCardTests, closeFieldTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import FieldModifications (putCard, closeField)
import Types (Field(..), Card(..), Suit(..), Player(..), FieldState(..))


putCardTests :: TestTree
putCardTests = testGroup "tests for putCard function"
  [
    testCase "just put card" $
      putCard [(Field [] [(Card Blue 10)] Open), (Field [(Card Yellow 3), (Card Blue 5)] [] Open), (Field [Card Yellow 1] [] Open)] 2 Two (Card Black 4) 
        @?= [(Field [] [(Card Blue 10)] Open), (Field [(Card Yellow 3), (Card Blue 5)] [(Card Black 4)] Open), (Field [Card Yellow 1] [] Open)]

  , testCase "already three" $
      putCard [(Field [(Card Yellow 3), (Card Blue 5), (Card Green 7)] [] Open)] 1 One (Card Black 10) 
        @?= [(Field [(Card Yellow 3), (Card Blue 5), (Card Green 7)] [] Open)]

  , testCase "closed field" $
      putCard [(Field [(Card Yellow 3), (Card Blue 5), (Card Green 7)] [] (Closed One))] 1 Two (Card Black 10) 
        @?= [(Field [(Card Yellow 3), (Card Blue 5), (Card Green 7)] [] (Closed One))]
  ]

closeFieldTests :: TestTree
closeFieldTests = testGroup "tests for closeField function"
  [
    testCase "just close" $
      closeField [(Field [] [(Card Blue 10)] Open), (Field [(Card Yellow 3), (Card Blue 5)] [] Open), (Field [Card Yellow 1] [] Open)] 2 One  
        @?= [(Field [] [(Card Blue 10)] Open), (Field [(Card Yellow 3), (Card Blue 5)] [] (Closed One)), (Field [Card Yellow 1] [] Open)]
  ]

