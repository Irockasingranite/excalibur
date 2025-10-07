module Checks.CommandCheck (
    performCommandCheck,
) where

import Control.Lens
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified Data.Text as T
import System.Process.Typed

import Types

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
