module Main (main) where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Data.Yaml
import Lens.Micro.Platform
import System.Process.Typed

import Types
import Util

performChecks :: CheckConfiguration -> IO ()
performChecks config = do
    inTempCopy "excalibur" $ \dir -> do
        putStrLn $ "Running checks in " ++ dir
        performGlobalChecks $ config ^. globalChecks

performGlobalChecks :: [Check] -> IO ()
performGlobalChecks checks = do
    results <- forM checks $ \c -> do
        putStrLn $ "Running check: " ++ (c ^. checkName & T.unpack)
        performCheck c
    print results

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
    let eConfig = decodeEither' (BS.pack configFile)
    case eConfig of
        Right config -> performChecks config
        Left e -> print e
