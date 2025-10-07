{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Optics
import Options.Applicative as O
import System.Exit

import Checks
import Types
import Util

data CmdLineOptions
    = CmdLineOptions
    { commits :: Text
    , configFile :: FilePath
    , outputFile :: FilePath
    , repository :: FilePath
    }

parseOptions :: Parser CmdLineOptions
parseOptions =
    CmdLineOptions
        -- Implements SPEC-3 @relation(SPEC-3, scope=range_start)
        <$> strOption
            ( long "commit-range"
                <> short 'r'
                <> help "Commit range to check"
                <> metavar "COMMITS"
                <> value "HEAD"
                <> showDefault
            )
        -- @relation(SPEC-3, scope=range_end)

        -- Implements SPEC-2 @relation(SPEC-2, scope=range_start)
        <*> strOption
            ( long "config"
                <> short 'c'
                <> help "Check configuration file"
                <> metavar "CONFIG"
                <> value "config.yaml"
                <> showDefault
            )
        -- @relation(SPEC-2, scope=range_end)

        <*> strOption
            ( long "output"
                <> short 'o'
                <> help "Output file"
                <> metavar "OUTPUT"
                <> value "results.json"
                <> showDefault
            )
        <*> O.argument
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

    let repoDir = options.repository
        configFile_ = options.configFile
        commitRange = options.commits
        outputFile_ = options.outputFile

    mCommits <- resolveCommitRange repoDir (T.unpack commitRange)

    when (isNothing mCommits) $ do
        putStrLn $ "Failed to resolve commit range " ++ T.unpack commitRange
        exitFailure

    let commits_ = fromMaybe [] mCommits

    configContents <- readFile configFile_
    let eConfig = Yaml.decodeEither' (BS.pack configContents) :: Either Yaml.ParseException CheckConfiguration
    print eConfig

    case eConfig of
        Left e -> print e
        Right config -> do
            report <- performChecks config repoDir commits_
            putStrLn $ replicate 40 '-'

            putStrLn $ summarizeReport report
            let encoded = JSON.encodePretty report & LBS.unpack
            writeFile outputFile_ encoded
