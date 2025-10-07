module Util.ResolveCommitRange (
    resolveCommitRange,
) where

import Data.ByteString.Lazy.Char8 as LBS (unpack)
import qualified Data.Text as T
import System.Process.Typed

import Types
import Util.RunCommand

-- Implements SPEC-6 @relation(SPEC-6, scope=file)

-- Resolves a commit range string into a list of revisions/commit IDs
resolveCommitRange :: FilePath -> String -> IO (Maybe [Commit])
resolveCommitRange repo range = do
    let cmd = "git rev-list " ++ range
    (exit, out) <- runCommandIn repo cmd
    case exit of
        ExitFailure _ -> return Nothing
        ExitSuccess -> return $ Just (parseOut out)
  where
    parseOut o = fmap T.pack (reverse . lines $ LBS.unpack o)
