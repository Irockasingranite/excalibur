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

import Checks.FileCheck
import Checks.GlobalCheck
import Types
import Util

-- Runs all checks given in a check configuration.
runChecks :: CheckConfiguration -> CheckVariables -> FilePath -> [Commit] -> IO Report
runChecks config vars repo commits = do
    inTempCopy repo "excalibur" $ \tmpDir -> do
        liftIO $ putStrLn $ "Running checks in " ++ tmpDir

        -- Global checks run only on final repository state
        -- Implements SPEC-11 @relation(SPEC-11, scope=range_start)
        let finalCommit = getFinal commits
            repoContext = CheckContext tmpDir finalCommit commits vars
            repoChecks = config.repoChecks
        liftIO $ checkoutCommit tmpDir finalCommit
        globalReports <- runReaderT (runChecksInContext repoChecks) repoContext
        -- @relation(SPEC-11, scope=range_end)

        -- Per-Commit checks run on each commit in the range
        -- Implements SPEC-14 @relation(SPEC-14, scope=range_start)
        commitReports <- forMDList commits $ \c -> do
            let context = CheckContext tmpDir c [c] vars
            let checks = config.commitChecks
            liftIO $ checkoutCommit tmpDir c
            liftIO $ putStrLn $ "Checking commit " ++ T.unpack c
            runReaderT (runChecksInContext checks) context
        -- @relation(SPEC-14, scope=range_end)

        -- Flatten nested DLists of reports into a single DList
        let allCommitReports = (DL.concat . DL.toList) commitReports
        return $
            Report
                { repoReports = globalReports
                , commitReports = allCommitReports
                }

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
        CheckFileCheck c -> runFileCheck c

