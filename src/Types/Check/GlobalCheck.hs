{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Types.Check.GlobalCheck where

import Data.Aeson
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml as Yaml
import System.Exit

import Types.Base

-- Implements SPEC-1 @relation(SPEC-1, scope=file)

-- Parse a JSON/YAML value into a valid ExitCode value.
parseExitCode :: Value -> Yaml.Parser ExitCode
parseExitCode (Number n) = case toBoundedInteger n of
    Just 0 -> return ExitSuccess
    Just nn -> return $ ExitFailure nn
    Nothing -> fail "Invalid exit code"
parseExitCode _ = fail "Invalid exit code"

-- Format an ExitCode value as a String
formatExitCode :: ExitCode -> String
formatExitCode e = case e of
    ExitSuccess -> "0"
    ExitFailure n -> show n

-- A Check based on an external command. Success or failure is determined by comparing the command's
-- exit to an expected value.
data GlobalCheck
    = GlobalCheck
    { name :: Text
    , command :: Command
    , expectedExit :: ExitCode
    }
    deriving (Show)

instance FromJSON GlobalCheck where
    parseJSON = withObject "GlobalCheck" $ \v -> do
        name <- v .: "name"
        cmd <- v .: "command"
        exit <- parseExitCode =<< (v .: "expected_exit")
        return $ GlobalCheck name cmd exit

instance ToJSON GlobalCheck where
    toJSON c =
        object
            [ "name" .= c.name
            , "command" .= c.command
            , "expected_exit" .= formatExitCode c.expectedExit
            ]

data GlobalCheckFailure
    = GlobalCheckFailure
    { expectedExit :: ExitCode
    , actualExit :: ExitCode
    , logs :: Text
    }

instance ToJSON GlobalCheckFailure where
    toJSON f =
        object
            [ "exitcode" .= formatExitCode f.actualExit
            , "logs" .= f.logs
            ]

instance Show GlobalCheckFailure where
    show f = unlines [summary, logs]
      where
        summary =
            "Unexpected exit code: "
                ++ formatExitCode f.actualExit
                ++ " (expected "
                ++ formatExitCode f.expectedExit
                ++ ")"
        logs = unlines ["Logs: ", T.unpack f.logs]
