{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Aeson
import Data.Aeson.Encode.Pretty as Pretty
import Data.DList
import Data.Scientific
import Data.Text (Text)
import Data.Yaml as Yaml
import Lens.Micro.Platform (makeLenses, (^.))
import System.Exit

type Command = Text

type Commit = Text

data Check
    = Check
    { _checkName :: Text
    , _checkCommand :: Command
    , _checkExpectedExit :: ExitCode
    }
    deriving (Show)

makeLenses ''Check

parseExitCode :: Value -> Yaml.Parser ExitCode
parseExitCode (Number n) = case toBoundedInteger n of
    Just 0 -> return ExitSuccess
    Just nn -> return $ ExitFailure nn
    Nothing -> fail "Invalid exit code"
parseExitCode _ = fail "Invalid exit code"

printExitCode :: ExitCode -> String
printExitCode e = case e of
    ExitSuccess -> "0"
    ExitFailure n -> show n

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

instance ToJSON Check where
    toJSON c =
        object
            [ "name" .= (c ^. checkName)
            , "command" .= (c ^. checkCommand)
            , "expected_exit" .= printExitCode (c ^. checkExpectedExit)
            ]

checkKeyCmp :: Text -> Text -> Ordering
checkKeyCmp = keyOrder ["name", "command", "expected_exit"]

data CheckResultType
    = CheckResultPassed
    | CheckResultFailed
    deriving (Eq)

instance Show CheckResultType where
    show CheckResultPassed = "Passed"
    show CheckResultFailed = "Failed"

instance ToJSON CheckResultType where
    toJSON c = case c of
        CheckResultPassed -> String "passed"
        CheckResultFailed -> String "failed"

data CheckResult
    = CheckResult
    { _resultCheck :: Check
    , _resultType :: CheckResultType
    , _resultOutput :: Text
    , _checkedCommit :: Commit
    }
    deriving (Show)

makeLenses ''CheckResult

instance ToJSON CheckResult where
    toJSON r =
        object
            [ "check" .= (r ^. resultCheck)
            , "commit" .= (r ^. checkedCommit)
            , "result" .= (r ^. resultType)
            , "output" .= (r ^. resultOutput)
            ]

checkResultKeyCmp :: Text -> Text -> Ordering
checkResultKeyCmp = keyOrder ["check", "commit", "result", "output", "error"] <> checkKeyCmp

checkResultPrettyConfig :: Pretty.Config
checkResultPrettyConfig =
    Pretty.Config
        { confIndent = Spaces 4
        , confCompare = checkResultKeyCmp
        , confNumFormat = Pretty.Generic
        , confTrailingNewline = False
        }

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

data Report
    = Report
    { _globalCheckResults :: DList CheckResult
    , _perCommitCheckResults :: DList CheckResult
    }

makeLenses ''Report

instance Semigroup Report where
    r1 <> r2 =
        Report
            (r1 ^. globalCheckResults <> r2 ^. globalCheckResults)
            (r1 ^. perCommitCheckResults <> r2 ^. perCommitCheckResults)

instance Monoid Report where
    mempty = Report mempty mempty

instance ToJSON Report where
    toJSON r =
        object
            [ "global" .= (r ^. globalCheckResults)
            , "per_commit" .= (r ^. perCommitCheckResults)
            ]

reportPrettyConfig :: Pretty.Config
reportPrettyConfig =
    Pretty.Config
        { confIndent = Spaces 4
        , confCompare = checkResultKeyCmp
        , confNumFormat = Pretty.Generic
        , confTrailingNewline = False
        }
