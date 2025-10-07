{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Scientific
import Data.Text (Text)
import Data.Yaml
import Lens.Micro.Platform
import System.Exit

type Command = Text

data Check
    = Check
    { _checkName :: Text
    , _checkCommand :: Command
    , _checkExpectedExit :: ExitCode
    }
    deriving (Show)

makeLenses ''Check

parseExitCode :: Value -> Parser ExitCode
parseExitCode (Number n) = case toBoundedInteger n of
    Just 0 -> return ExitSuccess
    Just nn -> return $ ExitFailure nn
    Nothing -> fail "Invalid exit code"
parseExitCode _ = fail "Invalid exit code"

instance FromJSON Check where
    parseJSON = withObject "Check" $ \v -> do
        name <- v .: "name"
        cmd <- v .: "command"
        exit <- (v .: "expected_exit") >>= parseExitCode
        return $
            Check
                { _checkName = name
                , _checkCommand = cmd
                , _checkExpectedExit = exit
                }

data CheckResultType
    = CheckResultPassed
    | CheckResultFailed
    deriving (Eq, Show)

data CheckResult
    = CheckResult
    { _resultCheck :: Check
    , _resultType :: CheckResultType
    , _resultOutput :: Text
    , _resultError :: Text
    }
    deriving (Show)

makeLenses ''CheckResult

data CheckConfiguration
    = CheckConfiguration
    { _globalChecks :: [Check]
    , _perCommitChecks :: [Check]
    }
    deriving (Show)

makeLenses ''CheckConfiguration

instance FromJSON CheckConfiguration where
    parseJSON = withObject "CheckConfiguration" $ \v -> do
        global <- v .: "global"
        perCommit <- v .: "per_commit"
        return $
            CheckConfiguration
                { _globalChecks = global
                , _perCommitChecks = perCommit
                }
