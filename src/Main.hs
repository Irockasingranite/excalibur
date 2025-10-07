module Main (main) where

import qualified Data.ByteString.Char8 as BS
import Data.Yaml

import Types
import Util

main :: IO ()
main = do
    inTempCopy "excalibur" $ \dir -> do
        configFile <- readFile "config.yaml"
        putStrLn "Read file:"
        putStrLn configFile
        let eChecks = decodeEither' (BS.pack configFile) :: Either ParseException CheckConfiguration
        print eChecks
