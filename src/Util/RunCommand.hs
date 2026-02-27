module Util.RunCommand (
    runCommandIn,
    runCommandWithStderrIn,
)
where

import Data.ByteString.Lazy.Char8
import System.Exit
import System.Process.Typed

runCommandIn :: FilePath -> String -> IO (ExitCode, ByteString)
runCommandIn wd cmd = do
    let cmd' = setWorkingDir wd $ shell cmd
    (exit, out, _) <- readProcess cmd'
    return (exit, out)

runCommandWithStderrIn :: FilePath -> String -> IO (ExitCode, ByteString)
runCommandWithStderrIn wd cmd = do
    let cmd' = setWorkingDir wd $ shell cmd
    (exit, out) <- readProcessInterleaved cmd'
    return (exit, out)
