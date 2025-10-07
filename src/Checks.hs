{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Checks (
    performChecks,
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.DList (DList)
import qualified Data.DList as DL
import qualified Data.Text as T

import Checks.CommandCheck
import Types
import Util

-- Runs all checks given in a check configuration.
performChecks :: CheckConfiguration -> FilePath -> [Commit] -> IO Report
performChecks config repo commits = do
    inTempCopy repo "excalibur" $ \dir -> do
        liftIO $ putStrLn $ "Running checks in " ++ dir

        -- Global checks run only on final repository state
        let finalCommit = getFinal commits
            globalContext = CheckContext repo finalCommit
            globalChecks = config.globalChecks
        liftIO $ checkoutCommit repo finalCommit
        globalReports <- runReaderT (performChecksInContext globalChecks) globalContext

        -- Per-Commit checks run on each commit in the range
        perCommitReports <- forMDList commits $ \c -> do
            let context = CheckContext repo c
            let checks = config.perCommitChecks
            liftIO $ checkoutCommit repo c
            liftIO $ putStrLn $ "Checking commit " ++ T.unpack c
            runReaderT (performChecksInContext checks) context

        -- Flatten nested DLists of reports into a single DList
        let allPerCommitReports = (DL.concat . DL.toList) perCommitReports
        return $
            Report
                { globalReports = globalReports
                , perCommitReports = allPerCommitReports
                }
  where
    getFinal [] = "HEAD" -- Use HEAD if no range is given
    getFinal [c] = c -- If one commit is given, use that one
    getFinal (_ : cs) = getFinal cs -- Recurse range to last one

-- Run a list of checks in a context. Assumes the right commit has been checked out.
performChecksInContext :: [Check] -> ReaderT CheckContext IO (DList CheckReport)
performChecksInContext checks = do
    forMDList checks $ \c -> do
        res <- performCheck c
        liftIO $ print res
        return res

-- Runs a single check in a context. Can read the context to fill out report details as needed.
performCheck :: Check -> ReaderT CheckContext IO CheckReport
performCheck check = do
    case check of
        CheckCommandCheck c -> performCommandCheck c
