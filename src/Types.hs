{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens (makeLenses, makePrisms, (^.))
import Data.Aeson
import Data.Aeson.KeyMap as KM
import Data.DList
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml as Yaml
import System.Exit

type Command = Text

type Commit = Text

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
        Types.Success -> "Success"
        Failure f -> "Failure: " ++ show f

instance ToJSON CheckResult where
    toJSON r = case r of
        Types.Success -> object ["result" .= String "Success"]
        Types.Failure f -> object ["result" .= String "Failure", "details" .= toJSON f]

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
