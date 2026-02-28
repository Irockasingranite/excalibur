{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GetFinalTests (getFinalTests) where

import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Util.ResolveCommitRange (getFinal)

getFinalTests :: TestTree
getFinalTests =
    testGroup
        "getFinal"
        [ testGroup "unit tests" unitTests
        , testGroup "properties" propertyTests
        ]

unitTests :: [TestTree]
unitTests =
    [ testCase "empty list returns HEAD" $
        getFinal [] @?= "HEAD"
    , testCase "singleton returns its element" $
        getFinal ["abc123"] @?= "abc123"
    , testCase "two elements returns the second" $
        getFinal ["first", "second"] @?= "second"
    , testCase "three elements returns the third" $
        getFinal ["a", "b", "c"] @?= "c"
    ]

propertyTests :: [TestTree]
propertyTests =
    [ QC.testProperty "non-empty list returns its last element" $
        QC.forAll (QC.listOf1 (QC.arbitrary :: QC.Gen String)) $ \cs ->
            getFinal (map T.pack cs) === T.pack (last cs)
    ]
