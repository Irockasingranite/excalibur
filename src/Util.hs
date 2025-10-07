{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Util (
    forMDList,
    inTempCopy, -- from Util.TempCopy
    resolveCommitRange, -- from Util.resolveCommitRange
    summarizeReport,
) where

import Control.Monad
import Data.DList (DList)
import qualified Data.DList as DL
import Optics

import Types
-- re-exported
import Util.ResolveCommitRange (resolveCommitRange)
import Util.TempCopy (inTempCopy)

summarizeReport :: Report -> String
summarizeReport report = unlines [repoSummary, commitSummary]
  where
    repoSummary = show repoPassed ++ "/" ++ show repoTotal ++ " repository checks passed"
    commitSummary = show commitPassed ++ "/" ++ show commitTotal ++ " commit checks passed"
    repoTotal = length report.repoReports
    repoPassed = length $ report ^. #repoReports ^.. traversed % #result % _Success
    commitTotal = length report.commitReports
    commitPassed = length $ report ^. #commitReports ^.. traversed % #result % _Success

-- Helper monad fold. Essentially a forM backed by a DList instead of a List.
forMDList :: (Monad m) => [a] -> (a -> m b) -> m (DList b)
forMDList xs f =
    foldM
        ( \acc x -> do
            b <- f x
            return (DL.snoc acc b)
        )
        DL.empty
        xs
