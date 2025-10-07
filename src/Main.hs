module Main (main) where

import Control.Monad
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Lens.Micro.Platform
import System.Process.Typed

import Types
import Util

performChecks :: CheckConfiguration -> IO [CheckResult]
performChecks config = do
    inTempCopy "excalibur" $ \dir -> do
        putStrLn $ "Running checks in " ++ dir
        performGlobalChecks $ config ^. globalChecks

performGlobalChecks :: [Check] -> IO [CheckResult]
performGlobalChecks checks = do
    forM checks $ \c -> do
        putStrLn $ "Running check: " ++ (c ^. checkName & T.unpack)
        performCheck c

performCheck :: Check -> IO CheckResult
performCheck c = do
    let cmd = c ^. checkCommand & T.unpack
        expected = c ^. checkExpectedExit
    (exit, out, err) <- readProcess (shell cmd)
    let result =
            if exit == expected
                then CheckResultPassed
                else CheckResultFailed
    return $
        CheckResult
            { _resultCheck = c
            , _resultOutput = (T.pack . LBS.unpack) out
            , _resultError = (T.pack . LBS.unpack) err
            , _resultType = result
            }

main :: IO ()
main = do
    configFile <- readFile "config.yaml"
    let eConfig = Yaml.decodeEither' (BS.pack configFile)
    case eConfig of
        Left e -> print e
        Right config -> do
            results <- performChecks config
            let encoded = Json.encodePretty' checkResultPrettyConfig results & LBS.unpack
            writeFile "results.json" encoded
