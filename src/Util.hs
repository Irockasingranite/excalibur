{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Util (
    forMDList,
    inTempCopy, -- from Util.TempCopy
    resolveCommitRange, -- from Util.resolveCommitRange
    mkReportSummary,
) where

import Control.Monad
import Data.DList (DList)
import qualified Data.DList as DL
import Optics

import Types

-- re-exported
import Util.ResolveCommitRange (resolveCommitRange)
import Util.TempCopy (inTempCopy)

mkReportSummary :: Report -> ReportSummary
mkReportSummary r =
    let repoTotal = length r.repoReports
        repoPassed = length $ r ^. #repoReports ^.. traversed % #result % _Success
        commitTotal = length r.commitReports
        commitPassed = length $ r ^. #commitReports ^.. traversed % #result % _Success
     in ReportSummary
            { repoChecksTotal = repoTotal
            , repoChecksPassed = repoPassed
            , repoChecksFailed = repoTotal - repoPassed
            , commitChecksTotal = commitTotal
            , commitChecksPassed = commitPassed
            , commitChecksFailed = commitTotal - commitPassed
            }

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
