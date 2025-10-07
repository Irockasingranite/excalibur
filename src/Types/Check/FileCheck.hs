{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Types.Check.FileCheck (
    FileCheck (..),
) where

import Data.Aeson
import Data.Text (Text)
import System.Exit

import Types.Base

-- Implements SPEC-1 @relation(SPEC-1, scope=file)

-- A Check based to be run on a set of files.
data FileCheck
    = FileCheck
    { name :: Text
    , command :: Command
    , expectedExit :: ExitCode
    , filePatterns :: [Text]
    , changedOnly :: Bool
    }
    deriving (Show)

instance Named FileCheck where
    showName c = show c.name

instance FromJSON FileCheck where
    parseJSON = withObject "FileCheck" $ \v -> do
        name <- v .: "name"
        cmd <- v .: "command"
        exit <- parseExitCode =<< (v .: "expected-exit")
        filePatterns <- v .: "files"
        changedOnly <- v .:? "changed-only" .!= False
        return $ FileCheck name cmd exit filePatterns changedOnly

instance ToJSON FileCheck where
    toJSON c =
        object
            [ "name" .= c.name
            , "command" .= c.command
            , "expected-exit" .= formatExitCode c.expectedExit
            , "files" .= c.filePatterns
            , "changed-only" .= c.changedOnly
            ]
