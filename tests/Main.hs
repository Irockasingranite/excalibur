module Main (main) where

import Test.Tasty

import ExpandVariablesTests (expandVariablesTests)
import ExitCodeTests (exitCodeTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "excalibur" [unitTests]

unitTests :: TestTree
unitTests =
    testGroup
        "Unit tests"
        [expandVariablesTests, exitCodeTests]
