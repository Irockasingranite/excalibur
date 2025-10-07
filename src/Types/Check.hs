{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Check
where

import Control.Lens (makeLenses, makePrisms, (^.))
import Data.Aeson
import Data.Aeson.KeyMap as KM
import Data.Scientific
import Data.Text (Text)
import Data.Yaml as Yaml
import System.Exit

import Types.Base

data FormatFileSpec
    = FormatFilesAll
    | FormatFilesChanged
    | FormatFilesSpecific [FilePath]
    deriving (Show)

data ClangFormatCheck
    = ClangFormatCheck
    { _clangFormatStyleFile :: FilePath
    , _clangFormatFileSpec :: FormatFileSpec
    }
    deriving (Show)

makeLenses ''ClangFormatCheck

instance FromJSON ClangFormatCheck where
    parseJSON = withObject "ClangFormatCheck" $ \v -> do
        styleFile <- v .: "style_file"
        let files = FormatFilesAll -- TODO: Parse FormatFileSpec
        return $ ClangFormatCheck styleFile files

instance ToJSON ClangFormatCheck where
    toJSON c =
        object
            [ "builtin" .= ("clang-format" :: String)
            , "style_file" .= (c ^. clangFormatStyleFile)
            , "files" .= ("undefined" :: String)
            ]

data CommandCheck
    = CommandCheck
    { _checkCommand :: Command
    , _checkExpectedExit :: ExitCode
    }
    deriving (Show)

makeLenses ''CommandCheck

parseExitCode :: Value -> Yaml.Parser ExitCode
parseExitCode (Number n) = case toBoundedInteger n of
    Just 0 -> return ExitSuccess
    Just nn -> return $ ExitFailure nn
    Nothing -> fail "Invalid exit code"
parseExitCode _ = fail "Invalid exit code"

formatExitCode :: ExitCode -> String
formatExitCode e = case e of
    ExitSuccess -> "0"
    ExitFailure n -> show n

instance FromJSON CommandCheck where
    parseJSON = withObject "CommandCheck" $ \v -> do
        cmd <- v .: "command"
        exit <- parseExitCode =<< (v .: "expected_exit")
        return $ CommandCheck cmd exit

instance ToJSON CommandCheck where
    toJSON c =
        object
            [ "command" .= (c ^. checkCommand)
            , "expected_exit" .= formatExitCode (c ^. checkExpectedExit)
            ]

data Check
    = CheckCommandCheck CommandCheck
    | CheckClangFormatCheck ClangFormatCheck
    deriving (Show)

makePrisms ''Check

instance FromJSON Check where
    parseJSON =
        withObject
            "Check"
            $ \o -> do
                mBuiltin <- o .:? "builtin" :: Parser (Maybe String)
                case mBuiltin of
                    Nothing -> CheckCommandCheck <$> parseJSON (Object o)
                    Just "clang-format" -> CheckClangFormatCheck <$> parseJSON (Object o)
                    _ -> fail "Unknown builtin check"

instance ToJSON Check where
    toJSON c = case c of
        CheckCommandCheck cc -> toJSON cc
        CheckClangFormatCheck cfc -> toJSON cfc

data NamedCheck
    = NamedCheck
    { _checkName :: Text
    , _checkInner :: Check
    }
    deriving (Show)

makeLenses ''NamedCheck

instance FromJSON NamedCheck where
    parseJSON =
        withObject "NamedCheck" $ \v -> do
            name <- v .: "name"
            check <- parseJSON (Object v)
            return $ NamedCheck name check

instance ToJSON NamedCheck where
    toJSON nc =
        let oCheck = toJSON (nc ^. checkInner)
            outer = KM.fromList ["name" .= (nc ^. checkName)]
         in case oCheck of
                Object check -> Object (outer `union` check)
                _ -> error "Invalid check encoding"

data CheckConfiguration
    = CheckConfiguration
    { _globalChecks :: [NamedCheck]
    , _perCommitChecks :: [NamedCheck]
    }
    deriving (Show)

makeLenses ''CheckConfiguration

instance FromJSON CheckConfiguration where
    parseJSON = withObject "CheckConfiguration" $ \o -> do
        global <- o .: "global"
        perCommit <- o .: "per_commit"
        return $ CheckConfiguration global perCommit
