{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExitCodeTests (exitCodeTests) where

import Data.Aeson (Value (..))
import Data.Scientific (scientific)
import qualified Data.ByteString.Char8 as BS
import Data.Yaml (decodeEither', parseEither)
import System.Exit

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Types.Base (formatExitCode, parseExitCode)

exitCodeTests :: TestTree
exitCodeTests =
    testGroup
        "exit codes"
        [ formatTests
        , parseTests
        , roundTripTests
        ]

-- Generator for ExitCode values that are canonical (i.e. 0 always maps to
-- ExitSuccess, never ExitFailure 0), matching what parseExitCode produces.
genExitCode :: QC.Gen ExitCode
genExitCode =
    QC.frequency
        [ (1, pure ExitSuccess)
        , (9, ExitFailure . QC.getNonZero <$> QC.arbitrary)
        ]

roundTripTests :: TestTree
roundTripTests =
    testGroup
        "round-trip"
        [ QC.testProperty "parse then format recovers the original integer" $
            -- For every Int n, parsing Number n then formatting gives show n.
            \(n :: Int) ->
                fmap formatExitCode (parseEither parseExitCode (Number (fromIntegral n)))
                    === Right (show n)
        , QC.testProperty "format then parse recovers the original ExitCode" $
            -- Simulates writing the formatted value to a YAML file and reading
            -- it back: decodeEither' parses the raw string into a Value, which
            -- is then fed to parseExitCode.
            -- ExitFailure 0 is excluded: the parser normalises 0 to ExitSuccess.
            QC.forAll genExitCode $ \ec ->
                case decodeEither' (BS.pack (formatExitCode ec)) of
                    Left err -> QC.counterexample ("YAML decode failed: " ++ show err) False
                    Right v  -> parseEither parseExitCode v === Right ec
        ]

formatTests :: TestTree
formatTests =
    testGroup
        "formatExitCode"
        [ testCase "ExitSuccess" $
            formatExitCode ExitSuccess @?= "0"
        , testCase "ExitFailure 1" $
            formatExitCode (ExitFailure 1) @?= "1"
        , testCase "ExitFailure 127" $
            formatExitCode (ExitFailure 127) @?= "127"
        , testCase "ExitFailure negative" $
            formatExitCode (ExitFailure (-1)) @?= "-1"
        ]

parseTests :: TestTree
parseTests =
    testGroup
        "parseExitCode"
        [ testCase "0 -> ExitSuccess" $
            parseEither parseExitCode (Number 0) @?= Right ExitSuccess
        , testCase "1 -> ExitFailure 1" $
            parseEither parseExitCode (Number 1) @?= Right (ExitFailure 1)
        , testCase "127 -> ExitFailure 127" $
            parseEither parseExitCode (Number 127) @?= Right (ExitFailure 127)
        , testCase "negative -> ExitFailure negative" $
            parseEither parseExitCode (Number (-1)) @?= Right (ExitFailure (-1))
        , testCase "fractional number is rejected" $
            -- scientific 15 (-1) = 1.5
            assertLeft $ parseEither parseExitCode (Number (scientific 15 (-1)))
        , testCase "out-of-bounds number is rejected" $
            -- scientific 1 100 = 10^100, far beyond maxBound :: Int
            assertLeft $ parseEither parseExitCode (Number (scientific 1 100))
        , testCase "string is rejected" $
            assertLeft $ parseEither parseExitCode (String "0")
        , testCase "bool is rejected" $
            assertLeft $ parseEither parseExitCode (Bool True)
        , testCase "null is rejected" $
            assertLeft $ parseEither parseExitCode Null
        ]

assertLeft :: Show a => Either String a -> Assertion
assertLeft (Left _) = pure ()
assertLeft (Right v) = assertFailure $ "expected parse failure, got: " ++ show v
