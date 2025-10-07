module Util.ResolvePaths (
    resolvePaths,
) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath.Glob

-- Implements SPEC-8 @relation(SPEC-8, scope=file)

-- Takes a number of patterns and a root directory, and returns a list of absolute paths matching
-- that pattern relative to the root.
resolvePaths :: [Text] -> FilePath -> IO [FilePath]
resolvePaths patterns root = do
    -- Compile strings into proper pattern objects
    let patterns' = fmap (compile . T.unpack) patterns
    -- Glob over root directory
    matches <- globDir patterns' root
    -- Flatten list of lists
    return $ join matches
