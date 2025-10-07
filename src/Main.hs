{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Lens.Micro.Platform
import Options.Applicative
import System.Exit

import Checks
import Types
import Util

data CmdLineOptions
    = CmdLineOptions
    { _optionsCommits :: Text
    , _optionsConfigFile :: FilePath
    , _optionsOutputFile :: FilePath
    , _optionsRepository :: FilePath
    }

makeLenses ''CmdLineOptions

parseOptions :: Parser CmdLineOptions
parseOptions =
    CmdLineOptions
        <$> strOption
            ( long "commit-range"
                <> short 'r'
                <> help "Commit range to check"
                <> metavar "COMMITS"
                <> value "HEAD"
                <> showDefault
            )
        <*> strOption
            ( long "config"
                <> short 'c'
                <> help "Check configuration file"
                <> metavar "CONFIG"
                <> value "config.yaml"
                <> showDefault
            )
        <*> strOption
            ( long "output"
                <> short 'o'
                <> help "Output file"
                <> metavar "OUTPUT"
                <> value "results.json"
                <> showDefault
            )
        <*> argument
            str
            ( metavar "REPOSITORY"
                <> value "."
                <> showDefault
            )

opts :: ParserInfo CmdLineOptions
opts =
    info
        (parseOptions <**> helper)
        ( fullDesc
            <> progDesc "Run checks on a range of commits"
            <> header "excalibur"
        )

main :: IO ()
main = do
    options <- execParser opts

    let repoDir = options ^. optionsRepository
        configFile = options ^. optionsConfigFile
        commitRange = options ^. optionsCommits
        outputFile = options ^. optionsOutputFile

    mCommits <- resolveCommitRange repoDir (T.unpack commitRange)

    when (isNothing mCommits) $ do
        putStrLn $ "Failed to resolve commit range " ++ T.unpack commitRange
        exitFailure

    let commits = fromMaybe [] mCommits

    configContents <- readFile configFile
    let eConfig = Yaml.decodeEither' (BS.pack configContents)
    case eConfig of
        Left e -> print e
        Right config -> do
            ((), results) <- runStateT (performChecks config repoDir commits) mempty
            putStrLn $ replicate 40 '-'
            putStrLn $ formatReport results
            let encoded = Json.encodePretty' checkResultPrettyConfig results & LBS.unpack
            writeFile outputFile encoded
