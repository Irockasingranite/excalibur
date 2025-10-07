{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Util (
    checkoutCommit,
    forMDList,
    inTempCopy, -- from Util.TempCopy
    resolveCommitRange,
    summarizeReport,
) where

import Control.Monad
import Data.ByteString.Lazy.Char8 as LBS (unpack)
import Data.DList (DList)
import qualified Data.DList as DL
import qualified Data.Text as T
import Optics
import System.Process.Typed

import Types
import Util.TempCopy (inTempCopy) -- re-exported

checkoutCommit :: FilePath -> Commit -> IO ()
checkoutCommit repo hash = do
    _ <- readProcess_ $ setWorkingDir repo $ shell ("git checkout " ++ T.unpack hash)
    return ()

-- Resolves a commit range string into a list of revisions/commit IDs
-- Implements SPEC-6 @relation(SPEC-6, scope=range_start)
resolveCommitRange :: FilePath -> String -> IO (Maybe [Commit])
resolveCommitRange repo range = do
    (exit, out, _err) <- readProcess $ setWorkingDir repo $ shell $ listCmd range
    case exit of
        ExitFailure _ -> return Nothing
        ExitSuccess -> return $ Just (parseOut out)
  where
    listCmd r = "git rev-list " ++ r
    parseOut o = fmap T.pack (reverse . lines $ LBS.unpack o)

-- @relation(SPEC-6, scope=range_end)

summarizeReport :: Report -> String
summarizeReport report = unlines [globalSummary, localSummary]
  where
    globalSummary = show globalPassed ++ "/" ++ show globalTotal ++ " global checks passed"
    localSummary = show perCommitPassed ++ "/" ++ show perCommitTotal ++ " per-commit checks passed"
    globalTotal = length report.globalReports
    globalPassed = length $ report ^. #globalReports ^.. traversed % #result % _Success
    perCommitTotal = length report.perCommitReports
    perCommitPassed = length $ report ^. #perCommitReports ^.. traversed % #result % _Success

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
