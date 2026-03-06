module Main (main) where

import Test.Tasty

import CheckoutCommitTests (checkoutCommitTests)
import GetChangedFilesTests (getChangedFilesTests)
import ResolveCommitRangeTests (resolveCommitRangeTests)
import ResolvePathsTests (resolvePathsTests)

main :: IO ()
main =
    defaultMain $
        testGroup
            "excalibur integration"
            [ checkoutCommitTests
            , getChangedFilesTests
            , resolveCommitRangeTests
            , resolvePathsTests
            ]
