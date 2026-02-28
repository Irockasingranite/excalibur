{-# LANGUAGE OverloadedStrings #-}

module ExpandVariablesTests (expandVariablesTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Types (CheckVariables (..))
import Util.ExpandVariables (expandVariables)

expandVariablesTests :: TestTree
expandVariablesTests =
    testGroup
        "expandVariables"
        [ testGroup "basic substitution" basicTests
        , testGroup "no substitution" noSubstitutionTests
        , testGroup "edge cases" edgeCaseTests
        , testGroup "expansion ordering" orderingTests
        ]

vars :: CheckVariables
vars = CheckVariables{filename = "main.c", commitRange = "HEAD~1..HEAD"}

basicTests :: [TestTree]
basicTests =
    [ testCase "${filename}" $
        expandVariables vars "${filename}"
            @?= "main.c"
    , testCase "${commit-range}" $
        expandVariables vars "${commit-range}"
            @?= "HEAD~1..HEAD"
    , testCase "both variables" $
        expandVariables vars "check ${filename} in ${commit-range}"
            @?= "check main.c in HEAD~1..HEAD"
    , testCase "repeated variable" $
        expandVariables vars "${filename} and ${filename}"
            @?= "main.c and main.c"
    , testCase "adjacent variables" $
        expandVariables vars "${filename}${commit-range}"
            @?= "main.cHEAD~1..HEAD"
    ]

noSubstitutionTests :: [TestTree]
noSubstitutionTests =
    [ testCase "plain text is unchanged" $
        expandVariables vars "no variables here"
            @?= "no variables here"
    , testCase "empty string" $
        expandVariables vars ""
            @?= ""
    , testCase "unknown variable is left as-is" $
        expandVariables vars "${unknown}"
            @?= "${unknown}"
    , testCase "partial variable name is left as-is" $
        expandVariables vars "${filenam}"
            @?= "${filenam}"
    , testCase "bare dollar sign is left as-is" $
        expandVariables vars "$filename"
            @?= "$filename"
    ]

edgeCaseTests :: [TestTree]
edgeCaseTests =
    [ testCase "empty filename" $
        expandVariables vars{filename = ""} "${filename}"
            @?= ""
    , testCase "empty commit range" $
        expandVariables vars{commitRange = ""} "${commit-range}"
            @?= ""
    , testCase "filename with path separators" $
        expandVariables vars{filename = "src/Util/Main.hs"} "${filename}"
            @?= "src/Util/Main.hs"
    , testCase "filename with spaces" $
        expandVariables vars{filename = "file with spaces.c"} "${filename}"
            @?= "file with spaces.c"
    ]

-- expandVariables = expandFilename . expandCommitRange
-- i.e. commit-range is substituted first, then filename.
-- These tests document the observable consequences of that order.
orderingTests :: [TestTree]
orderingTests =
    [ testCase "commit-range value containing ${filename} is further expanded" $
        -- ${commit-range} → "${filename}" → "main.c"
        expandVariables vars{commitRange = "${filename}"} "${commit-range}"
            @?= "main.c"
    , testCase "filename value containing ${commit-range} is NOT further expanded" $
        -- expandCommitRange runs first (no ${commit-range} in input here),
        -- then expandFilename replaces ${filename} with the literal "${commit-range}"
        expandVariables vars{filename = "${commit-range}"} "${filename}"
            @?= "${commit-range}"
    ]
