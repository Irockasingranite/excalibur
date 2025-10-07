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

module Types.CheckResult where

import Data.Aeson (KeyValue (..), ToJSON (..), Value (..), object)
import Data.Aeson.KeyMap as KM
import Data.DList
import qualified Data.Text as T
import Optics (makeFieldLabelsNoPrefix, makePrisms)

import Types.Base
import Types.Check
import Types.Check.GlobalCheck

data CheckFailure
    = CheckFailureGlobalCheck GlobalCheckFailure

instance Show CheckFailure where
    show f = case f of
        CheckFailureGlobalCheck ff -> show ff

instance ToJSON CheckFailure where
    toJSON f = case f of
        CheckFailureGlobalCheck cf -> toJSON cf

data CheckResult
    = Success
    | Failure CheckFailure

makePrisms ''CheckResult

instance Show CheckResult where
    show r = case r of
        Success -> "Success"
        Failure f -> "Failure: " ++ show f

-- @relation(SPEC-7, scope=range_start)
-- Encodes a check result as JSON. Depending on the result it may contain different fields.
instance ToJSON CheckResult where
    toJSON r = case r of
        Success -> object ["result" .= String "Success"]
        Failure f -> object ["result" .= String "Failure", "details" .= toJSON f]

-- @relation(SPEC-7, scope=range_end)

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
            ++ show r.check
            ++ " on "
            ++ T.unpack r.commit
            ++ ": "
            ++ show r.result

-- @relation(SPEC-7, scope=range_start)
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

-- @relation(SPEC-7, scope=range_end)

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

-- @relation(SPEC-7, scope=range_start)
-- Encodes the full report as JSON, grouping the reports by scope.
instance ToJSON Report where
    toJSON r =
        object
            [ "on-repository" .= r.repoReports
            , "on-commit" .= r.commitReports
            ]

-- @relation(SPEC-7, scope=range_end)
