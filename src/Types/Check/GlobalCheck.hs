{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Types.Check.GlobalCheck where

import Data.Aeson
import Data.Text (Text)
import System.Exit

import Types.Base

-- Implements SPEC-1 @relation(SPEC-1, scope=file)

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
