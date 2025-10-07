{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.CheckResult where

import Control.Lens (makeLenses, makePrisms, (^.))
import Data.Aeson (KeyValue (..), ToJSON (..), Value (..), object)
import Data.Aeson.KeyMap as KM
import Data.DList
import qualified Data.Text as T

import Types.Base
import Types.Check
import Types.Check.CommandCheck

data CheckFailure
    = CheckFailureCommandCheck CommandCheckFailure

makePrisms ''CheckFailure

instance Show CheckFailure where
    show f = case f of
        CheckFailureCommandCheck ff -> show ff

instance ToJSON CheckFailure where
    toJSON f = case f of
        CheckFailureCommandCheck cf -> toJSON cf

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
    { _reportCheck :: NamedCheck
    , _reportResult :: CheckResult
    , _reportCommit :: Commit
    }

makeLenses ''CheckReport

instance Show CheckReport where
    show r =
        "Check "
            ++ show (r ^. reportCheck . checkName)
            ++ " on "
            ++ T.unpack (r ^. reportCommit)
            ++ ": "
            ++ show (r ^. reportResult)

-- @relation(SPEC-7, scope=range_start)
-- Encodes a single check report as JSON. The result itself is encoded separately, and the result is
-- merged with the additional fields here.
instance ToJSON CheckReport where
    toJSON r =
        let oResult = toJSON (r ^. reportResult)
            outer =
                KM.fromList
                    [ "check" .= (r ^. reportCheck)
                    , "commit" .= (r ^. reportCommit)
                    ]
         in case oResult of
                Object result -> Object (outer `union` result)
                _ -> error "Invalid result encoding"

-- @relation(SPEC-7, scope=range_end)

data Report
    = Report
    { _globalReports :: DList CheckReport
    , _perCommitReports :: DList CheckReport
    }

makeLenses ''Report

instance Semigroup Report where
    (Report g1 p1) <> (Report g2 p2) = Report (g1 <> g2) (p1 <> p2)

instance Monoid Report where
    mempty = Report mempty mempty

-- @relation(SPEC-7, scope=range_start)
-- Encodes the full report as JSON, grouping the reports by scope.
instance ToJSON Report where
    toJSON r =
        object
            [ "global" .= (r ^. globalReports)
            , "per_commit" .= (r ^. perCommitReports)
            ]

-- @relation(SPEC-7, scope=range_end)
