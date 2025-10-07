{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Check.CommandCheck where

import Data.Aeson
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml as Yaml
import System.Exit

import Types.Base

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
-- Implements REQ-6. @relation(REQ-6, scope=range_start)
data CommandCheck
    = CommandCheck
    { command :: Command
    , expectedExit :: ExitCode
    }
    deriving (Show)

-- @relation(REQ-6, scope=range_end)

-- Implements SPEC-5 @relation(SPEC-5, scope=range_start)
instance FromJSON CommandCheck where
    parseJSON = withObject "CommandCheck" $ \v -> do
        cmd <- v .: "command"
        exit <- parseExitCode =<< (v .: "expected_exit")
        return $ CommandCheck cmd exit

-- @relation(SPEC-5, scope=range_end)

instance ToJSON CommandCheck where
    toJSON c =
        object
            [ "command" .= c.command
            , "expected_exit" .= formatExitCode c.expectedExit
            ]

data CommandCheckFailure
    = CommandCheckFailure
    { expectedExit :: ExitCode
    , actualExit :: ExitCode
    , logs :: Text
    }

instance ToJSON CommandCheckFailure where
    toJSON f =
        object
            [ "exitcode" .= formatExitCode f.actualExit
            , "logs" .= f.logs
            ]

instance Show CommandCheckFailure where
    show f = unlines [summary, logs]
      where
        summary =
            "Unexpected exit code: "
                ++ formatExitCode f.actualExit
                ++ " (expected "
                ++ formatExitCode f.expectedExit
                ++ ")"
        logs = unlines ["Logs: ", T.unpack f.logs]
