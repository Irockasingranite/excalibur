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
    let commit = getFinal commits
    inTempCopy repo "excalibur" $ \dir -> do
        liftIO $ putStrLn $ "Running checks in " ++ dir
        performGlobalChecks dir commit (config ^. globalChecks)
        forM_ commits $ \c -> performPerCommitChecks dir c (config ^. perCommitChecks)
  where
    getFinal [] = "HEAD"
    getFinal [c] = c
    getFinal (_ : cs) = getFinal cs

performGlobalChecks :: FilePath -> Commit -> [Check] -> StateT Report IO ()
performGlobalChecks wd commit checks = do
    forM_ checks $ \c -> do
        liftIO $ putStrLn $ "Running check: " ++ (c ^. checkName & T.unpack)
        result <- liftIO $ performCheck wd commit c
        liftIO $ putStrLn (formatCheckResult result)
        globalCheckResults %= flip DL.snoc result

performPerCommitChecks :: FilePath -> Commit -> [Check] -> StateT Report IO ()
performPerCommitChecks wd commit checks = do
    liftIO $ putStrLn $ "Checking commit " ++ T.unpack commit
    results <- forM checks $ \c -> do
        liftIO $ putStrLn $ "Running check: " ++ (c ^. checkName & T.unpack)
        result <- liftIO $ performCheckOnCommit commit wd c
        liftIO $ putStrLn (formatCheckResult result)
        return result

    perCommitCheckResults %= DL.append (DL.fromList results)

performCheckOnCommit :: Commit -> FilePath -> Check -> IO CheckResult
performCheckOnCommit commit wd check = do
    checkoutCommit wd commit
    performCheck wd commit check

performCheck :: FilePath -> Commit -> Check -> IO CheckResult
performCheck wd commit check = do
    let cmd = check ^. checkCommand & T.unpack
        expected = check ^. checkExpectedExit
    (exit, out) <- readProcessInterleaved (setWorkingDir wd $ shell cmd)
    let result =
            if exit == expected
                then CheckResultPassed
                else CheckResultFailed
    return $
        CheckResult
            { _resultCheck = check
            , _resultOutput = (T.pack . LBS.unpack) out
            , _resultType = result
            , _checkedCommit = commit
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
