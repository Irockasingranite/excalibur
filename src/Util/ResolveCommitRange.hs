module Util.ResolveCommitRange (
    resolveCommitRange,
) where

import Data.ByteString.Lazy.Char8 as LBS (unpack)
import qualified Data.Text as T
import System.Process.Typed

import Types

-- Implements SPEC-6 @relation(SPEC-6, scope=file)

-- Resolves a commit range string into a list of revisions/commit IDs
resolveCommitRange :: FilePath -> String -> IO (Maybe [Commit])
resolveCommitRange repo range = do
    (exit, out, _err) <- readProcess $ setWorkingDir repo $ shell $ listCmd range
    case exit of
        ExitFailure _ -> return Nothing
        ExitSuccess -> return $ Just (parseOut out)
  where
    listCmd r = "git rev-list " ++ r
    parseOut o = fmap T.pack (reverse . lines $ LBS.unpack o)
