{-# LANGUAGE OverloadedStrings #-}

module Checks (performChecks) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.DList as DL
import qualified Data.Text as T
import Lens.Micro.Platform
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

performGlobalChecks :: FilePath -> Commit -> [NamedCheck] -> StateT Report IO ()
performGlobalChecks wd commit checks = do
    forM_ checks $ \c -> do
        liftIO $ putStrLn $ "Running check: " ++ (c ^. checkName & T.unpack)
        result <- liftIO $ performCheck wd commit c
        liftIO $ putStrLn (formatCheckResult result)
        globalCheckResults %= flip DL.snoc result

performPerCommitChecks :: FilePath -> Commit -> [NamedCheck] -> StateT Report IO ()
performPerCommitChecks wd commit checks = do
    liftIO $ putStrLn $ "Checking commit " ++ T.unpack commit
    results <- forM checks $ \c -> do
        liftIO $ putStrLn $ "Running check: " ++ (c ^. checkName & T.unpack)
        result <- liftIO $ performCheckOnCommit commit wd c
        liftIO $ putStrLn (formatCheckResult result)
        return result

    perCommitCheckResults %= DL.append (DL.fromList results)

performCheckOnCommit :: Commit -> FilePath -> NamedCheck -> IO CheckResult
performCheckOnCommit commit wd check = do
    checkoutCommit wd commit
    performCheck wd commit check

performCheck :: FilePath -> Commit -> NamedCheck -> IO CheckResult
performCheck wd commit check = do
    let cmd = check ^. checkInner . checkCommand & T.unpack
        expected = check ^. checkInner . checkExpectedExit
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
