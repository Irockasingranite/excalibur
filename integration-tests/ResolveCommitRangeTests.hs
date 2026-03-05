{-# LANGUAGE OverloadedStrings #-}

module ResolveCommitRangeTests (resolveCommitRangeTests) where

import Data.Text (unpack)
import Test.Tasty
import Test.Tasty.HUnit

import GitFixture
import Util.ResolveCommitRange (resolveCommitRange)

resolveCommitRangeTests :: TestTree
resolveCommitRangeTests =
    testGroup
        "resolveCommitRange"
        [ testCase "returns Nothing for an invalid range" $
            withGitRepo $ \repo -> do
                _ <- addCommit repo "a.txt" "hello"
                result <- resolveCommitRange repo "nonexistent..alsonotreal"
                result @?= Nothing
        , testCase "returns a single commit for HEAD" $
            withGitRepo $ \repo -> do
                hash <- addCommit repo "a.txt" "hello"
                result <- resolveCommitRange repo "HEAD"
                result @?= Just [hash]
        , testCase "returns all commits oldest-first" $
            withGitRepo $ \repo -> do
                h1 <- addCommit repo "a.txt" "v1"
                h2 <- addCommit repo "b.txt" "v2"
                h3 <- addCommit repo "c.txt" "v3"
                result <- resolveCommitRange repo "HEAD"
                result @?= Just [h1, h2, h3]
        , testCase "returns only commits in the given range" $
            withGitRepo $ \repo -> do
                _ <- addCommit repo "a.txt" "v1"
                h2 <- addCommit repo "b.txt" "v2"
                h3 <- addCommit repo "c.txt" "v3"
                result <- resolveCommitRange repo "HEAD~2..HEAD"
                result @?= Just [h2, h3]
        , testCase "returns Nothing in a repo with no commits" $
            withGitRepo $ \repo -> do
                result <- resolveCommitRange repo "HEAD"
                result @?= Nothing
        , testCase "resolves a range between two commit hashes" $
            withGitRepo $ \repo -> do
                h1 <- addCommit repo "a.txt" "v1"
                h2 <- addCommit repo "b.txt" "v2"
                h3 <- addCommit repo "c.txt" "v3"
                let range = unpack h1 ++ ".." ++ unpack h3
                result <- resolveCommitRange repo range
                result @?= Just [h2, h3]
        , testCase "resolves a range ending at a commit hash" $
            withGitRepo $ \repo -> do
                _ <- addCommit repo "a.txt" "v1"
                h2 <- addCommit repo "b.txt" "v2"
                h3 <- addCommit repo "c.txt" "v3"
                _ <- addCommit repo "d.txt" "v4"
                result <- resolveCommitRange repo ("HEAD~3.." ++ unpack h3)
                result @?= Just [h2, h3]
        , testCase "resolves a branch name" $
            withGitRepo $ \repo -> do
                h1 <- addCommit repo "a.txt" "v1"
                h2 <- addCommit repo "b.txt" "v2"
                createBranch repo "test-branch"
                h3 <- addCommit repo "b.txt" "v3"
                result <- resolveCommitRange repo "test-branch..HEAD"
                result @?= Just [h3]
                -- Also verify the branch itself resolves
                branchResult <- resolveCommitRange repo "test-branch"
                branchResult @?= Just [h1, h2]
        , testCase "resolves a range from branch to branch" $
            withGitRepo $ \repo -> do
                _ <- addCommit repo "a.txt" "v1"
                createBranch repo "branch-a"
                h2 <- addCommit repo "b.txt" "v2"
                h3 <- addCommit repo "c.txt" "v3"
                createBranch repo "branch-b"
                result <- resolveCommitRange repo "branch-a..branch-b"
                result @?= Just [h2, h3]
        , testCase "resolves a range from hash to branch" $
            withGitRepo $ \repo -> do
                h1 <- addCommit repo "a.txt" "v1"
                h2 <- addCommit repo "b.txt" "v2"
                h3 <- addCommit repo "c.txt" "v3"
                createBranch repo "target"
                result <- resolveCommitRange repo (unpack h1 ++ "..target")
                result @?= Just [h2, h3]
        , testCase "resolves a range from branch to hash" $
            withGitRepo $ \repo -> do
                _ <- addCommit repo "a.txt" "v1"
                createBranch repo "base"
                h2 <- addCommit repo "b.txt" "v2"
                h3 <- addCommit repo "c.txt" "v3"
                result <- resolveCommitRange repo ("base.." ++ unpack h3)
                result @?= Just [h2, h3]
        ]
