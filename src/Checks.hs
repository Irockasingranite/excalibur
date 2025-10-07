{-# LANGUAGE OverloadedStrings #-}

module Checks (
    performChecks,
) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.DList (DList)
import qualified Data.DList as DL
import qualified Data.Text as T

import Checks.CommandCheck
import Types
import Util

-- Runs all checks given in a check configuration.
-- Implements SPEC-8 and SPEC-9.
performChecks :: CheckConfiguration -> FilePath -> [Commit] -> IO Report
performChecks config repo commits = do
    inTempCopy repo "excalibur" $ \dir -> do
        liftIO $ putStrLn $ "Running checks in " ++ dir

        -- @relation(SPEC-8, scope=range_start)
        -- Global checks run only on final repository state
        let finalCommit = getFinal commits
            globalContext = CheckContext repo finalCommit
            globalChecks_ = config ^. globalChecks
        liftIO $ checkoutCommit repo finalCommit
        globalReports_ <- runReaderT (performChecksInContext globalChecks_) globalContext
        -- @relation(SPEC-8, scope=range_end)

        -- @relation(SPEC-9, scope=range_start)
        -- Per-Commit checks run on each commit in the range
        perCommitReports_ <- forMDList commits $ \c -> do
            let context = CheckContext repo c
            let checks = config ^. perCommitChecks
            liftIO $ checkoutCommit repo c
            liftIO $ putStrLn $ "Checking commit " ++ T.unpack c
            runReaderT (performChecksInContext checks) context
        -- @relation(SPEC-9, scope=range_end)

        -- Flatten nested DLists of reports into a single DList
        let allPerCommitReports = (DL.concat . DL.toList) perCommitReports_
        return $
            Report
                { _globalReports = globalReports_
                , _perCommitReports = allPerCommitReports
                }
  where
    getFinal [] = "HEAD" -- Use HEAD if no range is given
    getFinal [c] = c -- If one commit is given, use that one
    getFinal (_ : cs) = getFinal cs -- Recurse range to last one

-- Run a list of checks in a context. Assumes the right commit has been checked out.
performChecksInContext :: [NamedCheck] -> ReaderT CheckContext IO (DList CheckReport)
performChecksInContext checks = do
    forMDList checks $ \c -> do
        result <- performCheck c
        liftIO $ print result
        return result

-- Runs a single check in a context. Can read the context to fill out report details as needed.
performCheck :: NamedCheck -> ReaderT CheckContext IO CheckReport
performCheck check = do
    let name = check ^. checkName
    case check ^. checkInner of
        CheckCommandCheck c -> performCommandCheck name c
