{-# LANGUAGE OverloadedStrings #-}

module CheckoutCommitTests (checkoutCommitTests) where

import qualified Data.Text as T
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import Util.CheckoutCommit (checkoutCommit)
import GitFixture
import Util.RunCommand (runCommandIn)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE

-- | Helper: read HEAD in the repo as strict Text.
getHead :: FilePath -> IO T.Text
getHead repo = do
    (_, out) <- runCommandIn repo "git rev-parse HEAD"
    return (T.pack . TL.unpack . TL.strip . TE.decodeUtf8 $ out)

-- | Helper: read file contents.
readRepoFile :: FilePath -> String -> IO String
readRepoFile repo filename = readFile (repo </> filename)

checkoutCommitTests :: TestTree
checkoutCommitTests =
    testGroup
        "checkoutCommit"
        [ testCase "checks out a specific commit by hash" $
            withGitRepo $ \repo -> do
                h1 <- addCommit repo "a.txt" "v1"
                _ <- addCommit repo "a.txt" "v2"
                checkoutCommit repo h1
                contents <- readRepoFile repo "a.txt"
                contents @?= "v1"
        , testCase "HEAD points to the checked-out commit" $
            withGitRepo $ \repo -> do
                h1 <- addCommit repo "a.txt" "v1"
                _ <- addCommit repo "b.txt" "v2"
                checkoutCommit repo h1
                headHash <- getHead repo
                headHash @?= h1
        , testCase "can check out back and forth between commits" $
            withGitRepo $ \repo -> do
                h1 <- addCommit repo "a.txt" "v1"
                h2 <- addCommit repo "a.txt" "v2"
                checkoutCommit repo h1
                c1 <- readRepoFile repo "a.txt"
                c1 @?= "v1"
                checkoutCommit repo h2
                c2 <- readRepoFile repo "a.txt"
                c2 @?= "v2"
        , testCase "checked-out commit reflects the right set of files" $
            withGitRepo $ \repo -> do
                h1 <- addCommit repo "a.txt" "hello"
                _ <- addCommit repo "b.txt" "world"
                checkoutCommit repo h1
                -- b.txt should not exist at h1
                result <- runCommandIn repo "test -f b.txt && echo exists || echo missing"
                let output = T.pack . TL.unpack . TL.strip . TE.decodeUtf8 $ snd result
                output @?= "missing"
        , testCase "works after checking out a branch then a commit" $
            withGitRepo $ \repo -> do
                h1 <- addCommit repo "a.txt" "v1"
                createBranch repo "feature"
                checkoutBranch repo "feature"
                _ <- addCommit repo "a.txt" "v2"
                checkoutCommit repo h1
                contents <- readRepoFile repo "a.txt"
                contents @?= "v1"
        ]
