module Util (
    checkoutCommit,
    formatCheckResult,
    formatReport,
    inTempCopy,
    resolveCommitRange,
) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8 as LBS (unpack)
import qualified Data.DList as DL
import qualified Data.Text as T
import Lens.Micro.Platform
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Process.Typed

import Types

copyTree :: FilePath -> FilePath -> IO ()
copyTree src dst = do
    isDir <- doesDirectoryExist src
    if isDir
        then do
            createDirectoryIfMissing True dst
            entries <- listDirectory src
            forM_ entries $ \name -> do
                let s = src </> name
                    d = dst </> name
                isSubDir <- doesDirectoryExist s
                isFile <- doesFileExist s
                if isSubDir
                    then copyTree s d
                    else when isFile (copyFile s d)
        else do
            isFile <- doesFileExist src
            when isFile $ do
                createDirectoryIfMissing True (takeDirectory dst)
                copyFile src dst

inTempCopy :: (MonadIO m, MonadMask m) => FilePath -> String -> (FilePath -> m a) -> m a
inTempCopy dir prefix action = do
    cwd <- liftIO getCurrentDirectory
    withTempDirectory cwd prefix $ \tmpDir -> do
        let tmpName = takeFileName tmpDir
        entries <- liftIO $ listDirectory dir
        let toCopy = filter (/= tmpName) entries
        forM_ toCopy $ \name -> do
            let src = dir </> name
                dst = tmpDir </> name
            isDir <- liftIO $ doesDirectoryExist src
            if isDir
                then liftIO $ copyTree src dst
                else liftIO $ copyFile src dst

        action tmpDir

checkoutCommit :: FilePath -> Commit -> IO ()
checkoutCommit repo hash = do
    _ <- readProcess_ $ setWorkingDir repo $ shell ("git checkout " ++ T.unpack hash)
    return ()

resolveCommitRange :: FilePath -> String -> IO (Maybe [Commit])
resolveCommitRange repo range = do
    (exit, out, _err) <- readProcess $ setWorkingDir repo $ shell $ listCmd range
    case exit of
        ExitFailure _ -> return Nothing
        ExitSuccess -> return $ Just (parseOut out)
  where
    listCmd r = "git rev-list " ++ r
    parseOut o = fmap T.pack (reverse . lines $ LBS.unpack o)

formatCheckResult :: CheckResult -> String
formatCheckResult c = case c ^. resultType of
    CheckResultPassed -> statusline
    CheckResultFailed -> unlines [statusline, outputline]
  where
    name = c ^. resultCheck . checkName & T.unpack
    status = show (c ^. resultType)
    statusline = name ++ ": " ++ status
    outputline = "Output:\n" ++ T.unpack (c ^. resultOutput)

formatReport :: Report -> String
formatReport r = unlines [globalSummary, perCommitSummary]
  where
    nGlobalChecks = length . DL.toList $ r ^. globalCheckResults
    globalFailures = r ^. globalCheckResults & DL.toList & filter (\e -> (e ^. resultType) == CheckResultFailed)
    nGlobalFailures = length globalFailures
    nGlobalPassed = nGlobalChecks - nGlobalFailures
    globalSummary = "Global checks: " ++ show nGlobalPassed ++ "/" ++ show nGlobalChecks ++ " passed"
    allPerCommitChecks = r ^. perCommitCheckResults
    nPerCommitChecks = length allPerCommitChecks
    failedPerCommitChecks = reportOnlyFailures r ^. perCommitCheckResults
    nPerCommitFailures = length failedPerCommitChecks
    nPerCommitPassed = nPerCommitChecks - nPerCommitFailures
    perCommitSummary = "Per-commit checks: " ++ show nPerCommitPassed ++ "/" ++ show nPerCommitChecks ++ " passed"

reportOnlyFailures :: Report -> Report
reportOnlyFailures r =
    Report globalFiltered perCommitFiltered
  where
    isFailure x = (x ^. resultType) == CheckResultFailed
    globalFiltered = r ^. globalCheckResults ^.. traverse . filtered isFailure & DL.fromList
    perCommitFiltered = r ^. perCommitCheckResults ^.. traverse . filtered isFailure & DL.fromList
