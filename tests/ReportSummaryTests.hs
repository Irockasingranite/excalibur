{-# LANGUAGE OverloadedRecordDot #-}

module ReportSummaryTests (reportSummaryTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC hiding (isSuccess, Success, Failure)

import Types
import Util (mkReportSummary)

import CheckReportTests (genCheckResult, reportFrom, stubFailure)

reportSummaryTests :: TestTree
reportSummaryTests =
    testGroup
        "mkReportSummary"
        [ testGroup "unit tests" unitTests
        , testGroup "properties" propertyTests
        ]

isSuccessResult :: CheckResult -> Bool
isSuccessResult Success = True
isSuccessResult (Failure _) = False

-- Unit tests ------------------------------------------------------------------

unitTests :: [TestTree]
unitTests =
    [ testCase "empty report gives all zeros" $ do
        let summary = mkReportSummary (reportFrom [] [])
        summary.repoChecksTotal @?= 0
        summary.repoChecksPassed @?= 0
        summary.repoChecksFailed @?= 0
        summary.commitChecksTotal @?= 0
        summary.commitChecksPassed @?= 0
        summary.commitChecksFailed @?= 0
    , testCase "all successes have zero failures" $ do
        let summary = mkReportSummary (reportFrom [Success, Success] [Success])
        summary.repoChecksFailed @?= 0
        summary.commitChecksFailed @?= 0
    , testCase "all failures have zero passes" $ do
        let f = Failure stubFailure
        let summary = mkReportSummary (reportFrom [f, f] [f])
        summary.repoChecksPassed @?= 0
        summary.commitChecksPassed @?= 0
    , testCase "repo and commit scopes are counted independently" $ do
        let f = Failure stubFailure
        let summary = mkReportSummary (reportFrom [Success, f, f] [Success, Success, f])
        summary.repoChecksTotal @?= 3
        summary.repoChecksPassed @?= 1
        summary.repoChecksFailed @?= 2
        summary.commitChecksTotal @?= 3
        summary.commitChecksPassed @?= 2
        summary.commitChecksFailed @?= 1
    ]

-- Properties ------------------------------------------------------------------

propertyTests :: [TestTree]
propertyTests =
    [ QC.testProperty "totals equal list lengths" $
        QC.forAll (QC.listOf genCheckResult) $ \repoReports ->
            QC.forAll (QC.listOf genCheckResult) $ \commitReports ->
                let summary = mkReportSummary (reportFrom repoReports commitReports)
                 in QC.conjoin
                        [ summary.repoChecksTotal QC.=== length repoReports
                        , summary.commitChecksTotal QC.=== length commitReports
                        ]
    , QC.testProperty "passed counts equal number of successes" $
        QC.forAll (QC.listOf genCheckResult) $ \repoReports ->
            QC.forAll (QC.listOf genCheckResult) $ \commitReports ->
                let summary = mkReportSummary (reportFrom repoReports commitReports)
                 in QC.conjoin
                        [ summary.repoChecksPassed QC.=== length (filter isSuccessResult repoReports)
                        , summary.commitChecksPassed QC.=== length (filter isSuccessResult commitReports)
                        ]
    , QC.testProperty "total equals passed plus failed" $
        QC.forAll (QC.listOf genCheckResult) $ \repoReports ->
            QC.forAll (QC.listOf genCheckResult) $ \commitReports ->
                let summary = mkReportSummary (reportFrom repoReports commitReports)
                 in QC.conjoin
                        [ summary.repoChecksTotal QC.=== summary.repoChecksPassed + summary.repoChecksFailed
                        , summary.commitChecksTotal QC.=== summary.commitChecksPassed + summary.commitChecksFailed
                        ]
    ]
