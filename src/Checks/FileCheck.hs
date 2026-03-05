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

import qualified Data.ByteString.Lazy.Char8 as BS
import System.Process.Typed
import Types
import Util.ExpandVariables
import Util.GetChangedFiles
import Util.ResolvePaths
import Util.RunCommand

-- Implements SPEC-13 @relation(SPEC-13, scope=file)

runFileCheck :: FileCheck -> ReaderT CheckContext IO CheckReport
runFileCheck check = do
    -- Grab context
    ctx <- ask

    -- Find files
    files <- liftIO $ resolvePaths check.filePatterns ctx.directory

    -- Optionally filter for changed files
    mAllChanged <- liftIO $ getChangedFiles ctx.directory ctx.commitRange
    let changedFiles = case mAllChanged of
            Nothing -> []
            Just allChanged -> filter (`elem` allChanged) files

    let filesToCheck =
            if check.changedOnly
                then changedFiles
                else files

    -- Run command on each file and collect output
    results <- forM filesToCheck $ \file -> do
        let cmdRaw = check.command
            vars' = withFilename file ctx.variables
            cmd = T.unpack . expandVariables vars' $ cmdRaw
        liftIO $ runCommandWithStderrIn ctx.directory cmd

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

    let result = if aggregateExit == check.expectedExit
            then Success
            else
                Failure $
                    CheckFailure
                        { expectedExit = check.expectedExit
                        , actualExit = aggregateExit
                        , logs = (T.pack . BS.unpack) allLogs
                        }

    return $
        CheckReport
            { check = CheckFileCheck check
            , commit = ctx.commit
            , result = result
            }
