module Util.RunCommand (
    runCommandIn,
    runCommandIn_,
    runCommandWithStderrIn,
)
where

import Control.Monad
import Data.ByteString.Lazy.Char8
import System.Exit
import System.Process.Typed

{- | Run a shell command in the given working directory, returning the exit code
and stdout. Stderr is discarded.
-}
runCommandIn :: FilePath -> String -> IO (ExitCode, ByteString)
runCommandIn wd cmd = do
    let cmd' = setWorkingDir wd $ shell cmd
    (exit, out, _) <- readProcess cmd'
    return (exit, out)

-- | Like 'runCommandIn', but discards the result.
runCommandIn_ :: FilePath -> String -> IO ()
runCommandIn_ = (void .) . runCommandIn

{- | Run a shell command in the given working directory, returning the exit code
and interleaved stdout/stderr.
-}
runCommandWithStderrIn :: FilePath -> String -> IO (ExitCode, ByteString)
runCommandWithStderrIn wd cmd = do
    let cmd' = setWorkingDir wd $ shell cmd
    (exit, out) <- readProcessInterleaved cmd'
    return (exit, out)
