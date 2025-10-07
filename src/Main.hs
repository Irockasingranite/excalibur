{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe
import qualified Data.Yaml as Yaml
import Lens.Micro.Platform
import System.Environment
import System.Exit

import Checks
import Types
import Util

main :: IO ()
main = do
    args <- getArgs
    when (length args < 2) $ do
        putStrLn "Usage: excalibur <repository> <commits>"
        exitFailure

    [repoDir, commitArg] <- getArgs
    mCommits <- resolveCommitRange repoDir commitArg
    when (isNothing mCommits) $ do
        putStrLn $ "Failed to resolve commit range " ++ commitArg
        exitFailure

    let commits = fromMaybe [] mCommits
    configFile <- readFile "config.yaml"
    let eConfig = Yaml.decodeEither' (BS.pack configFile)
    case eConfig of
        Left e -> print e
        Right config -> do
            ((), results) <- runStateT (performChecks config repoDir commits) mempty
            putStrLn $ replicate 40 '-'
            putStrLn $ formatReport results
            let encoded = Json.encodePretty' checkResultPrettyConfig results & LBS.unpack
            writeFile "results.json" encoded
