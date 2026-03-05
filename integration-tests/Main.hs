module Main (main) where

import Test.Tasty

import ResolveCommitRangeTests (resolveCommitRangeTests)

main :: IO ()
main = defaultMain $ testGroup "excalibur integration" [resolveCommitRangeTests]
