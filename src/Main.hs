{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.DList as DL
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Lens.Micro.Platform
import System.Environment
import System.Exit
import System.Process.Typed

import Types
import Util

performChecks :: CheckConfiguration -> FilePath -> [Commit] -> StateT Report IO ()
performChecks config repo commits = do
    inTempCopy repo "excalibur" $ \dir -> do
        liftIO $ putStrLn $ "Running checks in " ++ dir
        performGlobalChecks dir (config ^. globalChecks)
        forM_ commits $ \c -> performPerCommitChecks c dir (config ^. perCommitChecks)

performGlobalChecks :: FilePath -> [Check] -> StateT Report IO ()
performGlobalChecks wd checks = do
    forM_ checks $ \c -> do
        liftIO $ putStrLn $ "Running check: " ++ (c ^. checkName & T.unpack)
        result <- liftIO $ performCheck wd c
        liftIO $ putStrLn (formatCheckResult result)
        globalCheckResults %= flip DL.snoc result

performPerCommitChecks :: Commit -> FilePath -> [Check] -> StateT Report IO ()
performPerCommitChecks hash wd checks = do
    liftIO $ putStrLn $ "Checking commit " ++ T.unpack hash
    results <- forM checks $ \c -> do
        liftIO $ putStrLn $ "Running check: " ++ (c ^. checkName & T.unpack)
        result <- liftIO $ performCheckOnCommit hash wd c
        liftIO $ putStrLn (formatCheckResult result)
        return result

    let commitResult = CommitReport hash (DL.fromList results)
    perCommitCheckResults %= flip DL.snoc commitResult

performCheckOnCommit :: Commit -> FilePath -> Check -> IO CheckResult
performCheckOnCommit hash wd check = do
    checkoutCommit wd hash
    performCheck wd check

performCheck :: FilePath -> Check -> IO CheckResult
performCheck wd c = do
    let cmd = c ^. checkCommand & T.unpack
        expected = c ^. checkExpectedExit
    (exit, out) <- readProcessInterleaved (setWorkingDir wd $ shell cmd)
    let result =
            if exit == expected
                then CheckResultPassed
                else CheckResultFailed
    return $
        CheckResult
            { _resultCheck = c
            , _resultOutput = (T.pack . LBS.unpack) out
            , _resultType = result
            }

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
