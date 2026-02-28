module Main (main) where

import Test.Tasty

import CheckReportTests (checkReportTests)
import ExitCodeTests (exitCodeTests)
import ExpandVariablesTests (expandVariablesTests)
import GetFinalTests (getFinalTests)
import ReportSummaryTests (reportSummaryTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "excalibur" [unitTests]

unitTests :: TestTree
unitTests =
    testGroup
        "Unit tests"
        [checkReportTests, expandVariablesTests, exitCodeTests, getFinalTests, reportSummaryTests]
