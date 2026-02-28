{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module CheckReportTests (
    checkReportTests,
    stubFailure,
    stubReport,
    genCheckResult,
    genCheckReport,
    genReport,
    reportFrom,
) where

import qualified Data.DList as DL
import System.Exit

import Test.Tasty
import Test.Tasty.QuickCheck as QC hiding (isSuccess, Success, Failure)

import Types

-- Stubs -----------------------------------------------------------------------

stubFailure :: CheckFailure
stubFailure = CheckFailure{expectedExit = ExitSuccess, actualExit = ExitFailure 1, logs = ""}

stubReport :: CheckResult -> CheckReport
stubReport r =
    CheckReport
        { check = CheckGlobalCheck (GlobalCheck{name = "", command = "", expectedExit = ExitSuccess})
        , result = r
        , commit = ""
        }

-- | Build a Report from two lists of CheckResults, using stub values for all
-- CheckReport fields that mkReportSummary does not inspect.
reportFrom :: [CheckResult] -> [CheckResult] -> Report
reportFrom repoReports commitReports =
    Report
        { repoReports = DL.fromList (map stubReport repoReports)
        , commitReports = DL.fromList (map stubReport commitReports)
        }

-- Generators ------------------------------------------------------------------

genCheckResult :: QC.Gen CheckResult
genCheckResult = QC.elements [Success, Failure stubFailure]

genCheckReport :: QC.Gen CheckReport
genCheckReport = stubReport <$> genCheckResult

genReport :: QC.Gen Report
genReport =
    Report
        <$> (DL.fromList <$> QC.listOf genCheckReport)
        <*> (DL.fromList <$> QC.listOf genCheckReport)

-- Tests -----------------------------------------------------------------------

checkReportTests :: TestTree
checkReportTests =
    testGroup
        "Report Monoid"
        [ QC.testProperty "left identity" $
            QC.forAll genReport $ \r ->
                mempty <> r === r
        , QC.testProperty "right identity" $
            QC.forAll genReport $ \r ->
                r <> mempty === r
        , QC.testProperty "associativity" $
            QC.forAll genReport $ \r1 ->
            QC.forAll genReport $ \r2 ->
            QC.forAll genReport $ \r3 ->
                (r1 <> r2) <> r3 === r1 <> (r2 <> r3)
        ]
