{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Checks (
    performChecks,
) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Text (Text)
import qualified Data.Text as T
import System.Process.Typed

import Types
import Util

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

-- Context a check runs in
data CheckContext
    = CheckContext
    { _contextDirectory :: FilePath
    , _contextCommit :: Commit
    }

makeLenses ''CheckContext

performChecks :: CheckConfiguration -> FilePath -> [Commit] -> IO Report
performChecks config repo commits = do
    inTempCopy repo "excalibur" $ \dir -> do
        liftIO $ putStrLn $ "Running checks in " ++ dir

        -- Global checks run only on final repository state
        let finalCommit = getFinal commits
            globalContext = CheckContext repo finalCommit
            globalChecks_ = config ^. globalChecks
        globalReports_ <- runReaderT (performGlobalChecks globalChecks_) globalContext

        -- Per-Commit checks run on each commit in the range
        perCommitReports_ <- forMDList commits $ \c -> do
            let context = CheckContext repo c
            let checks = config ^. perCommitChecks
            runReaderT (performPerCommitChecks checks) context

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

performGlobalChecks :: [NamedCheck] -> ReaderT CheckContext IO (DList CheckReport)
performGlobalChecks checks = do
    -- Use helper fold to aggregate reports directly into a DList.
    forMDList checks $ \c -> do
        result <- performCheck c
        liftIO $ print result
        return result

performPerCommitChecks :: [NamedCheck] -> ReaderT CheckContext IO (DList CheckReport)
performPerCommitChecks checks = do
    wd <- view contextDirectory
    commit <- view contextCommit
    liftIO $ putStrLn $ "Checking commit " ++ T.unpack commit
    forMDList checks $ \c -> do
        liftIO $ checkoutCommit wd commit
        result <- performCheck c
        liftIO $ print result
        return result

performCheck :: NamedCheck -> ReaderT CheckContext IO CheckReport
performCheck check = do
    let name = check ^. checkName
    case check ^. checkInner of
        CheckCommandCheck c -> performCommandCheck name c

performCommandCheck :: Text -> CommandCheck -> ReaderT CheckContext IO CheckReport
performCommandCheck name check = do
    wd <- view contextDirectory
    commit <- view contextCommit
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
