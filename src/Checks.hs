{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Checks (
    runChecks,
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.DList (DList)
import qualified Data.DList as DL
import qualified Data.Text as T
import System.Process.Typed

import Checks.GlobalCheck
import Types
import Util

-- Runs all checks given in a check configuration.
runChecks :: CheckConfiguration -> FilePath -> [Commit] -> IO Report
runChecks config repo commits = do
    inTempCopy repo "excalibur" $ \tmpDir -> do
        liftIO $ putStrLn $ "Running checks in " ++ tmpDir

        -- Global checks run only on final repository state
        let finalCommit = getFinal commits
            repoContext = CheckContext tmpDir finalCommit
            repoChecks = config.repoChecks
        liftIO $ checkoutCommit repo finalCommit
        globalReports <- runReaderT (runChecksInContext repoChecks) repoContext

        -- Per-Commit checks run on each commit in the range
        commitReports <- forMDList commits $ \c -> do
            let context = CheckContext tmpDir c
            let checks = config.commitChecks
            liftIO $ checkoutCommit tmpDir c
            liftIO $ putStrLn $ "Checking commit " ++ T.unpack c
            runReaderT (runChecksInContext checks) context

        -- Flatten nested DLists of reports into a single DList
        let allCommitReports = (DL.concat . DL.toList) commitReports
        return $
            Report
                { repoReports = globalReports
                , commitReports = allCommitReports
                }
  where
    getFinal [] = "HEAD" -- Use HEAD if no range is given
    getFinal [c] = c -- If one commit is given, use that one
    getFinal (_ : cs) = getFinal cs -- Recurse range to last one

-- Run a list of checks in a context. Assumes the right commit has been checked out.
runChecksInContext :: [Check] -> ReaderT CheckContext IO (DList CheckReport)
runChecksInContext checks = do
    forMDList checks $ \c -> do
        res <- runCheck c
        liftIO $ print res
        return res

-- Runs a single check in a context. Can read the context to fill out report details as needed.
runCheck :: Check -> ReaderT CheckContext IO CheckReport
runCheck check = do
    case check of
        CheckGlobalCheck c -> runGlobalCheck c

-- Checks out a specific commit in a directory
checkoutCommit :: FilePath -> Commit -> IO ()
checkoutCommit repo hash = do
    _ <- readProcess_ $ setWorkingDir repo $ shell ("git checkout " ++ T.unpack hash)
    return ()
