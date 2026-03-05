{-# LANGUAGE OverloadedStrings #-}

module GetChangedFilesTests (getChangedFilesTests) where

import Data.List (sort)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import Util.GetChangedFiles (getChangedFiles)
import GitFixture

getChangedFilesTests :: TestTree
getChangedFilesTests =
    testGroup
        "getChangedFiles"
        [ testCase "empty commit list diffs HEAD against HEAD~1" $
            withGitRepo $ \repo -> do
                _ <- addCommit repo "a.txt" "v1"
                _ <- addCommit repo "b.txt" "v2"
                result <- getChangedFiles repo []
                result @?= Just [repo </> "b.txt"]
        , testCase "single commit returns that commit's changes" $
            withGitRepo $ \repo -> do
                _ <- addCommit repo "a.txt" "v1"
                h2 <- addCommit repo "b.txt" "v2"
                _ <- addCommit repo "c.txt" "v3"
                result <- getChangedFiles repo [h2]
                result @?= Just [repo </> "b.txt"]
        , testCase "multiple commits returns changes across the range" $
            withGitRepo $ \repo -> do
                h1 <- addCommit repo "a.txt" "v1"
                _ <- addCommit repo "b.txt" "v2"
                h3 <- addCommit repo "c.txt" "v3"
                result <- getChangedFiles repo [h1, h3]
                -- Diff from h1 to h3: b.txt and c.txt were added
                fmap sort result @?= Just (sort [repo </> "b.txt", repo </> "c.txt"])
        , testCase "detects modified files" $
            withGitRepo $ \repo -> do
                _ <- addCommit repo "a.txt" "v1"
                h2 <- modifyFile repo "a.txt" "v2"
                result <- getChangedFiles repo [h2]
                result @?= Just [repo </> "a.txt"]
        , testCase "returns absolute paths" $
            withGitRepo $ \repo -> do
                _ <- addCommit repo "a.txt" "v1"
                _ <- addCommit repo "b.txt" "v2"
                result <- getChangedFiles repo []
                case result of
                    Just (p : _) -> assertBool "path should start with repo dir" (take (length repo) p == repo)
                    _ -> assertFailure "expected Just with at least one path"
        , testCase "returns Nothing for an invalid commit" $
            withGitRepo $ \repo -> do
                _ <- addCommit repo "a.txt" "v1"
                result <- getChangedFiles repo ["deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"]
                result @?= Nothing
        , testCase "no changes between identical commits returns empty list" $
            withGitRepo $ \repo -> do
                h1 <- addCommit repo "a.txt" "v1"
                result <- getChangedFiles repo [h1, h1]
                result @?= Just []
        ]
