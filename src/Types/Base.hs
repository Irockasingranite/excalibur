module Types.Base (
    Command,
    Commit,
    formatExitCode,
    parseExitCode,
) where

import Data.Scientific
import Data.Text (Text)
import Data.Yaml as Yaml
import System.Exit

type Command = Text
type Commit = Text

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
