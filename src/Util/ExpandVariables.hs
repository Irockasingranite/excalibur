{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- Implements SPEC-10 @relation(SPEC-10, scope=file)

module Util.ExpandVariables (
    expandVariables,
    withFilename,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Types

expandVariables :: CheckVariables -> Text -> Text
expandVariables vars = expandFilename . expandCommitRange
  where
    expandFilename = T.replace "${filename}" (T.pack vars.filename)
    expandCommitRange = T.replace "${commit-range}" vars.commitRange

withFilename :: FilePath -> CheckVariables -> CheckVariables
withFilename filename before =
    CheckVariables filename before.commitRange
