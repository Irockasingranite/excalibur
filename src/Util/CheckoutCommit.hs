{-# LANGUAGE OverloadedStrings #-}

module Util.CheckoutCommit (
    checkoutCommit,
) where

import Control.Monad
import qualified Data.Text as T

import Types.Base (Commit)
import Util.RunCommand (runCommandIn)

-- Checks out a specific commit in a directory
checkoutCommit :: FilePath -> Commit -> IO ()
checkoutCommit repo hash =
    void $ runCommandIn repo $ "git checkout " ++ T.unpack hash
