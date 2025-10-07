module Util.RunCommand (
    runCommandIn,
    runCommandWithStderrIn,
)
where

import Control.Monad.Trans.Reader
import Data.ByteString.Lazy.Char8
import System.Exit
import System.Process.Typed

import Types

runCommandIn :: FilePath -> String -> IO (ExitCode, ByteString)
runCommandIn wd cmd = do
    let cmd' = setWorkingDir wd $ shell cmd
    (exit, out, _) <- readProcess cmd'
    return (exit, out)

runCommandWithStderrIn :: FilePath -> String -> ReaderT CheckContext IO (ExitCode, ByteString)
runCommandWithStderrIn wd cmd = do
    let cmd' = setWorkingDir wd $ shell cmd
    (exit, out) <- readProcessInterleaved cmd'
    return (exit, out)
