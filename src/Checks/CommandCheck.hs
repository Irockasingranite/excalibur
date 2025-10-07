{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Checks.CommandCheck (
    performCommandCheck,
) where

-- Implements SPEC-5 @relation(SPEC-5, scope=file)

import Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Optics
import System.Process.Typed

import Types

performCommandCheck :: CommandCheck -> ReaderT CheckContext IO CheckReport
performCommandCheck check = do
    wd <- asks (view #directory)
    commit <- asks (view #commit)
    let cmd = check.command & T.unpack
        expected = check.expectedExit
    (exit, out) <- readProcessInterleaved (setWorkingDir wd $ shell cmd)
    let result =
            if exit == expected
                then Success
                else
                    Failure $
                        CheckFailureCommandCheck $
                            CommandCheckFailure
                                { expectedExit = expected
                                , actualExit = exit
                                , logs = (T.pack . LBS.unpack) out
                                }
    return $
        CheckReport
            { check = CheckCommandCheck check
            , commit = commit
            , result = result
            }
