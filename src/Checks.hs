{-# LANGUAGE OverloadedStrings #-}

module Checks (
    performChecks,
) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.DList as DL
import Data.Text (Text)
import qualified Data.Text as T
import System.Process.Typed

import Types
import Util

performChecks :: CheckConfiguration -> FilePath -> [Commit] -> StateT Report IO ()
performChecks config repo commits = do
    let commit = getFinal commits
    inTempCopy repo "excalibur" $ \dir -> do
        liftIO $ putStrLn $ "Running checks in " ++ dir
        performGlobalChecks dir commit (config ^. globalChecks)
        forM_ commits $ \c -> performPerCommitChecks dir c (config ^. perCommitChecks)
  where
    getFinal [] = "HEAD"
    getFinal [c] = c
    getFinal (_ : cs) = getFinal cs

performGlobalChecks :: FilePath -> Commit -> [NamedCheck] -> StateT Report IO ()
performGlobalChecks wd commit checks = do
    forM_ checks $ \c -> do
        result <- liftIO $ performCheck wd commit c
        liftIO $ print result
        globalReports %= flip DL.snoc result

performPerCommitChecks :: FilePath -> Commit -> [NamedCheck] -> StateT Report IO ()
performPerCommitChecks wd commit checks = do
    liftIO $ putStrLn $ "Checking commit " ++ T.unpack commit
    results <- forM checks $ \c -> do
        result <- liftIO $ performCheckOnCommit commit wd c
        liftIO $ print result
        return result

    perCommitReports %= DL.append (DL.fromList results)

performCheckOnCommit :: Commit -> FilePath -> NamedCheck -> IO CheckReport
performCheckOnCommit commit wd check = do
    checkoutCommit wd commit
    performCheck wd commit check

performCheck :: FilePath -> Commit -> NamedCheck -> IO CheckReport
performCheck wd commit check = do
    let name = check ^. checkName
    case check ^. checkInner of
        CheckCommandCheck c -> performCommandCheck wd commit c name
        _ -> undefined

performCommandCheck :: FilePath -> Commit -> CommandCheck -> Text -> IO CheckReport
performCommandCheck wd commit check name = do
    let cmd = check ^. checkCommand & T.unpack
        expected = check ^. checkExpectedExit
    (exit, out) <- readProcessInterleaved (setWorkingDir wd $ shell cmd)
    let result =
            if exit == expected
                then Success
                else
                    Failure $
                        CheckFailureCommandCheck $
                            CommandCheckFailure
                                { _checkFailureExpectedExit = expected
                                , _checkFailureActualExit = exit
                                , _checkFailureLogs = (T.pack . LBS.unpack) out
                                }
    return $
        CheckReport
            { _reportCheck = NamedCheck name (CheckCommandCheck check)
            , _reportCommit = commit
            , _reportResult = result
            }
