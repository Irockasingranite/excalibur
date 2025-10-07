module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.DList as DL
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Lens.Micro.Platform
import System.Process.Typed

import Types
import Util

performChecks :: CheckConfiguration -> StateT Report IO ()
performChecks config = do
    inTempCopy "excalibur" $ \dir -> do
        liftIO $ putStrLn $ "Running checks in " ++ dir
        performGlobalChecks dir (config ^. globalChecks)

performGlobalChecks :: FilePath -> [Check] -> StateT Report IO ()
performGlobalChecks wd checks = do
    forM_ checks $ \c -> do
        liftIO $ putStrLn $ "Running check: " ++ (c ^. checkName & T.unpack)
        result <- liftIO $ performCheck wd c
        globalCheckResults %= flip snoc result

performCheck :: FilePath -> Check -> IO CheckResult
performCheck wd c = do
    let cmd = c ^. checkCommand & T.unpack
        expected = c ^. checkExpectedExit
    (exit, out, err) <- readProcess (setWorkingDir wd $ shell cmd)
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
            ((), results) <- runStateT (performChecks config) mempty
            let encoded = Json.encodePretty' checkResultPrettyConfig results & LBS.unpack
            writeFile "results.json" encoded
