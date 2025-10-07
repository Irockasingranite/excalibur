{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Checks.FileCheck (
    runFileCheck,
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.List (foldl')
import qualified Data.Text as T
import Optics
import System.FilePath

import qualified Data.ByteString.Lazy.Char8 as BS
import System.Process.Typed
import Types
import Util.ResolvePaths

-- Implements SPEC-1 @relation(SPEC-1, scope=file)

runFileCheck :: FileCheck -> ReaderT CheckContext IO CheckReport
runFileCheck check = do
    wd <- asks (view #directory)
    commit <- asks (view #commit)
    commitRange <- asks (view #commitRange)

    -- Find files
    files <- liftIO $ resolvePaths check.filePatterns wd

    -- Optionally filter for changed files
    mAllChanged <- liftIO $ getChangedFiles wd commitRange
    let changedFiles = case mAllChanged of
            Nothing -> []
            Just allChanged -> filter (`elem` allChanged) files

    let filesToCheck =
            if check.changedOnly
                then changedFiles
                else files

    -- Run command on each file and collect output
    results <- forM filesToCheck $ \file -> do
        let cmd = check.command & T.unpack
            process = setWorkingDir wd $ shell (cmd ++ " " ++ file)
        (exit, logs) <- readProcessInterleaved process
        return (exit, logs)

    -- Aggregate results:
    -- For Exit codes a single failure fails the whole check
    -- => Do a fold that returns the first failure, if any
    let aggregateExits acc x = case acc of
            ExitFailure f -> ExitFailure f
            ExitSuccess -> case x of
                ExitFailure f -> ExitFailure f
                ExitSuccess -> ExitSuccess
    -- exit code is 1st tuple member
    let allExits = results ^.. traversed % _1
    let aggregateExit = foldl' aggregateExits ExitSuccess allExits

    -- For logs we append them all together
    -- logs are 2nd tuple member
    let allLogs = results ^.. traversed % _2 & BS.intercalate (BS.pack "\n")

    let result = case aggregateExit of
            ExitSuccess -> Success
            ExitFailure _ ->
                Failure $
                    CheckFailure
                        { expectedExit = check.expectedExit
                        , actualExit = aggregateExit
                        , logs = (T.pack . BS.unpack) allLogs
                        }

    return $
        CheckReport
            { check = CheckFileCheck check
            , commit = commit
            , result = result
            }

getChangedFiles :: FilePath -> [Commit] -> IO (Maybe [FilePath])
getChangedFiles repo commits = do
    let (commitFrom, commitTo) = case commits of
            [] -> ("HEAD", "HEAD~1")
            [c] -> (show c ++ "~1", show c)
            (c : cs) -> (show c, (show . getFinal) cs)

    -- Ask git for list of changed filenames
    let cmd = "git diff --name-only " ++ show commitFrom ++ " " ++ show commitTo
        process = setWorkingDir repo $ shell cmd
    (exit, out, _) <- liftIO $ readProcess process

    case exit of
        ExitFailure _ -> return Nothing
        ExitSuccess -> do
            -- Output should be \n-separated list of files
            let files = (lines . BS.unpack) out
            -- Add directory to paths, since git doesn't include it
            return $ Just $ fmap (repo </>) files
  where
    getFinal :: [Commit] -> Commit
    getFinal [] = T.pack "HEAD"
    getFinal [c] = c
    getFinal (_ : cs) = getFinal cs
