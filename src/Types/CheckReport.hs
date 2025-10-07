{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Types.CheckReport where

import Data.Aeson (KeyValue (..), ToJSON (..), Value (..), object)
import Data.Aeson.KeyMap as KM
import Data.DList
import Data.Text (Text)
import qualified Data.Text as T
import Optics (makeFieldLabelsNoPrefix, makePrisms)
import System.Exit

import Types.Base
import Types.Check

-- Implements SPEC-7 @relation(SPEC-7, scope=file)

data CheckFailure
    = CheckFailure
    { expectedExit :: ExitCode
    , actualExit :: ExitCode
    , logs :: Text
    }

instance ToJSON CheckFailure where
    toJSON f =
        object
            [ "exitcode" .= formatExitCode f.actualExit
            , "logs" .= f.logs
            ]

instance Show CheckFailure where
    show f = unlines [summary, logs]
      where
        summary =
            "Unexpected exit code: "
                ++ formatExitCode f.actualExit
                ++ " (expected "
                ++ formatExitCode f.expectedExit
                ++ ")"
        logs = unlines ["Logs: ", T.unpack f.logs]

data CheckResult
    = Success
    | Failure CheckFailure

makePrisms ''CheckResult

instance Show CheckResult where
    show r = case r of
        Success -> "Success"
        Failure f -> "Failure: " ++ show f

-- Encodes a check result as JSON. Depending on the result it may contain different fields.
instance ToJSON CheckResult where
    toJSON r = case r of
        Success -> object ["result" .= String "Success"]
        Failure f ->
            object
                [ "result" .= String "Failure"
                , "details" .= toJSON f
                ]

data CheckReport
    = CheckReport
    { check :: Check
    , result :: CheckResult
    , commit :: Commit
    }

makeFieldLabelsNoPrefix ''CheckReport

instance Show CheckReport where
    show r =
        "Check "
            ++ showName r.check
            ++ " on "
            ++ T.unpack r.commit
            ++ ": "
            ++ show r.result

-- Encodes a single check report as JSON. The result itself is encoded separately, and the result is
-- merged with the additional fields here.
instance ToJSON CheckReport where
    toJSON r =
        let oResult = toJSON r.result
            outer =
                KM.fromList
                    [ "check" .= r.check
                    , "commit" .= r.commit
                    ]
         in case oResult of
                Object res -> Object (outer `union` res)
                _ -> error "Invalid result encoding"

data Report
    = Report
    { repoReports :: DList CheckReport
    , commitReports :: DList CheckReport
    }

makeFieldLabelsNoPrefix ''Report

instance Semigroup Report where
    (Report g1 p1) <> (Report g2 p2) = Report (g1 <> g2) (p1 <> p2)

instance Monoid Report where
    mempty = Report mempty mempty

-- Encodes the full report as JSON, grouping the reports by scope.
instance ToJSON Report where
    toJSON r =
        object
            [ "on-repository" .= r.repoReports
            , "on-commit" .= r.commitReports
            ]

data ReportSummary
    = ReportSummary
    { repoChecksTotal :: Int
    , repoChecksPassed :: Int
    , repoChecksFailed :: Int
    , commitChecksTotal :: Int
    , commitChecksPassed :: Int
    , commitChecksFailed :: Int
    }

makeFieldLabelsNoPrefix ''ReportSummary

instance Show ReportSummary where
    show s = unlines [repoSummary, commitSummary]
      where
        repoSummary =
            show s.repoChecksPassed
                ++ "/"
                ++ show s.repoChecksTotal
                ++ " repository checks passed"
        commitSummary =
            show s.commitChecksPassed
                ++ "/"
                ++ show s.commitChecksTotal
                ++ " commit checks passed"
