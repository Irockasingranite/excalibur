{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Check.CommandCheck where

import Control.Lens (makeLenses, makePrisms, (^.))
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
    { _checkCommand :: Command
    , _checkExpectedExit :: ExitCode
    }
    deriving (Show)

-- @relation(REQ-6, scope=range_end)

makeLenses ''CommandCheck

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
            [ "command" .= (c ^. checkCommand)
            , "expected_exit" .= formatExitCode (c ^. checkExpectedExit)
            ]

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
