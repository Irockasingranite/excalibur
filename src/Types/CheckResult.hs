{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.CheckResult where

import Control.Lens (makeLenses, makePrisms, (^.))
import Data.Aeson (KeyValue (..), ToJSON (..), Value (..), object)
import Data.Aeson.KeyMap as KM
import Data.DList
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit

import Types.Base
import Types.Check

data CommandCheckFailure
    = CommandCheckFailure
    { _checkFailureExpectedExit :: ExitCode
    , _checkFailureActualExit :: ExitCode
    , _checkFailureLogs :: Text
    }

makeLenses ''CommandCheckFailure

instance ToJSON CommandCheckFailure where
    toJSON f =
        object
            [ "exitcode" .= formatExitCode (f ^. checkFailureActualExit)
            , "logs" .= (f ^. checkFailureLogs)
            ]

instance Show CommandCheckFailure where
    show f = unlines [summary, logs]
      where
        summary =
            "Unexpected exit code: "
                ++ formatExitCode (f ^. checkFailureActualExit)
                ++ " (expected "
                ++ formatExitCode (f ^. checkFailureExpectedExit)
                ++ ")"
        logs = unlines ["Logs: ", T.unpack (f ^. checkFailureLogs)]

data ClangFormatCheckFailure
    = ClangFormatCheckFailure

instance ToJSON ClangFormatCheckFailure where
    toJSON _f = object []

instance Show ClangFormatCheckFailure where
    show _f = "Format check failed" -- TODO

makeLenses ''ClangFormatCheckFailure

data CheckFailure
    = CheckFailureCommandCheck CommandCheckFailure
    | CheckFailureClangFormatCheck ClangFormatCheckFailure

makePrisms ''CheckFailure

instance Show CheckFailure where
    show f = case f of
        CheckFailureCommandCheck ff -> show ff
        CheckFailureClangFormatCheck ff -> show ff

instance ToJSON CheckFailure where
    toJSON f = case f of
        CheckFailureCommandCheck cf -> toJSON cf
        CheckFailureClangFormatCheck cfcf -> toJSON cfcf

data CheckResult
    = Success
    | Failure CheckFailure

makePrisms ''CheckResult

instance Show CheckResult where
    show r = case r of
        Success -> "Success"
        Failure f -> "Failure: " ++ show f

instance ToJSON CheckResult where
    toJSON r = case r of
        Success -> object ["result" .= String "Success"]
        Failure f -> object ["result" .= String "Failure", "details" .= toJSON f]

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

instance ToJSON Report where
    toJSON r =
        object
            [ "global" .= (r ^. globalReports)
            , "per_commit" .= (r ^. perCommitReports)
            ]
