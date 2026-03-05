module Util.GetChangedFiles (
    getChangedFiles,
) where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import System.FilePath
import System.Process.Typed

import Types
import Util.ResolveCommitRange (getFinal)
import Util.RunCommand

getChangedFiles :: FilePath -> [Commit] -> IO (Maybe [FilePath])
getChangedFiles repo commits = do
    let (commitFrom, commitTo) = case commits of
            [] -> ("HEAD", "HEAD~1")
            [c] -> (T.unpack c ++ "~1", T.unpack c)
            (c : cs) -> (T.unpack c, (T.unpack . getFinal) cs)

    -- Ask git for list of changed filenames
    let cmd = "git diff --name-only " ++ commitFrom ++ " " ++ commitTo
    (exit, out) <- runCommandIn repo cmd

    case exit of
        ExitFailure _ -> return Nothing
        ExitSuccess -> do
            -- Output should be \n-separated list of files
            let files = (lines . BS.unpack) out
            -- Add directory to paths, since git doesn't include it
            return $ Just $ fmap (repo </>) files
