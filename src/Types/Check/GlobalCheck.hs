{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Types.Check.GlobalCheck (
    GlobalCheck (..),
) where

import Data.Aeson
import Data.Text (Text)
import System.Exit

import Types.Base

-- Implements SPEC-1 @relation(SPEC-1, scope=file)

-- A Check based command to be run on the entire repository.
data GlobalCheck
    = GlobalCheck
    { name :: Text
    , command :: Command
    , expectedExit :: ExitCode
    }
    deriving (Eq, Show)

instance Named GlobalCheck where
    showName c = show c.name

instance FromJSON GlobalCheck where
    parseJSON = withObject "GlobalCheck" $ \v -> do
        name <- v .: "name"
        cmd <- v .: "command"
        exit <- parseExitCode =<< (v .: "expected-exit")
        return $ GlobalCheck name cmd exit

instance ToJSON GlobalCheck where
    toJSON c =
        object
            [ "name" .= c.name
            , "command" .= c.command
            , "expected-exit" .= formatExitCode c.expectedExit
            ]
