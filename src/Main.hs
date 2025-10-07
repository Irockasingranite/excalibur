module Main (main) where

import Util

main :: IO ()
main = do
  inTempCopy "excalibur" $ \dir -> do
    print dir
