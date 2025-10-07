{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Checks.GlobalCheck (
    runGlobalCheck,
) where

import Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Optics

import Types
import Util.ExpandVariables
import Util.RunCommand

-- Implements SPEC-1 @relation(SPEC-1, scope=file)

runGlobalCheck :: GlobalCheck -> ReaderT CheckContext IO CheckReport
runGlobalCheck check = do
    wd <- asks (view #directory)
    commit <- asks (view #commit)
    vars <- asks (view #variables)
    let cmdRaw = check.command
        cmd = T.unpack . expandVariables vars $ cmdRaw
        expected = check.expectedExit
    (exit, out) <- runCommandWithStderrIn wd cmd
    let result =
            if exit == expected
                then Success
                else
                    Failure $
                        CheckFailure
                            { expectedExit = expected
                            , actualExit = exit
                            , logs = (T.pack . LBS.unpack) out
                            }
    return $
        CheckReport
            { check = CheckGlobalCheck check
            , commit = commit
            , result = result
            }
