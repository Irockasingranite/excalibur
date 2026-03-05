module Main (main) where

import Test.Tasty

import GetChangedFilesTests (getChangedFilesTests)
import ResolveCommitRangeTests (resolveCommitRangeTests)

main :: IO ()
main = defaultMain $ testGroup "excalibur integration" [getChangedFilesTests, resolveCommitRangeTests]
