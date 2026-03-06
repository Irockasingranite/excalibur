{-# LANGUAGE OverloadedStrings #-}

module ResolvePathsTests (resolvePathsTests) where

import Data.List (sort)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Util.ResolvePaths (resolvePaths)

-- | Creates a temp directory and runs an action in it.
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "excalibur-resolve-paths"

-- | Helper: create a file with content in a directory.
createFile :: FilePath -> String -> String -> IO ()
createFile dir name content = do
    let path = dir </> name
    createDirectoryIfMissing True (dir </> takeDirectory name)
    writeFile path content
  where
    takeDirectory = reverse . dropWhile (/= '/') . reverse

resolvePathsTests :: TestTree
resolvePathsTests =
    testGroup
        "resolvePaths"
        [ testCase "matches a single file by exact name" $
            withTempDir $ \dir -> do
                createFile dir "a.txt" ""
                result <- resolvePaths ["a.txt"] dir
                result @?= [dir </> "a.txt"]
        , testCase "matches files by wildcard extension" $
            withTempDir $ \dir -> do
                createFile dir "a.txt" ""
                createFile dir "b.txt" ""
                createFile dir "c.hs" ""
                result <- resolvePaths ["*.txt"] dir
                sort result @?= sort [dir </> "a.txt", dir </> "b.txt"]
        , testCase "matches files in subdirectories with **" $
            withTempDir $ \dir -> do
                createFile dir "src/Foo.hs" ""
                createFile dir "src/Bar.hs" ""
                createFile dir "test/Baz.hs" ""
                result <- resolvePaths ["**/*.hs"] dir
                sort result @?= sort [dir </> "src/Bar.hs", dir </> "src/Foo.hs", dir </> "test/Baz.hs"]
        , testCase "non-matching pattern returns empty list" $
            withTempDir $ \dir -> do
                createFile dir "a.txt" ""
                result <- resolvePaths ["*.hs"] dir
                result @?= []
        , testCase "multiple patterns combine results" $
            withTempDir $ \dir -> do
                createFile dir "a.txt" ""
                createFile dir "b.hs" ""
                createFile dir "c.py" ""
                result <- resolvePaths ["*.txt", "*.hs"] dir
                sort result @?= sort [dir </> "a.txt", dir </> "b.hs"]
        , testCase "empty pattern list returns empty list" $
            withTempDir $ \dir -> do
                createFile dir "a.txt" ""
                result <- resolvePaths [] dir
                result @?= []
        , testCase "returns absolute paths" $
            withTempDir $ \dir -> do
                createFile dir "a.txt" ""
                result <- resolvePaths ["a.txt"] dir
                case result of
                    ['/':_] -> return ()
                    [_]     -> assertFailure "path is not absolute"
                    _ -> assertFailure "expected exactly one result"
        , testCase "does not match files in parent directory" $
            withTempDir $ \dir -> do
                let sub = dir </> "sub"
                createDirectoryIfMissing True sub
                createFile dir "a.txt" ""
                createFile sub "b.txt" ""
                result <- resolvePaths ["*.txt"] sub
                result @?= [sub </> "b.txt"]
        ]
