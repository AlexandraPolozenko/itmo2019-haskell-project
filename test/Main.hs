module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)
import FieldModificationTests (putCardTests, closeFieldTests)
import WinnerChecksTests (comboTests, fightTests, checkWinnerTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [putCardTests, closeFieldTests, comboTests, fightTests, checkWinnerTests]