{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Types.Check where

import Control.Applicative
import Data.Aeson
import Data.Text (Text)
import Data.Yaml as Yaml
import Optics.TH

import Types.Base
import Types.Check.FileCheck
import Types.Check.GlobalCheck

-- Implements SPEC-1 @relation(SPEC-1, scope=file)

-- A check to be performed.
data Check
    = CheckGlobalCheck GlobalCheck
    | CheckFileCheck FileCheck
    deriving (Show)

instance Named Check where
    showName check = case check of
        CheckGlobalCheck c -> showName c
        CheckFileCheck c -> showName c

-- Parse into a specific check variant. Try parsing into a FileCheck first. If that fails, try as a
-- global check.
instance FromJSON Check where
    parseJSON v =
        (CheckFileCheck <$> (parseJSON v :: Parser FileCheck))
            <|> (CheckGlobalCheck <$> (parseJSON v :: Parser GlobalCheck))

instance ToJSON Check where
    toJSON c = case c of
        CheckGlobalCheck cc -> toJSON cc
        CheckFileCheck cc -> toJSON cc

-- A complete check configuration.
data CheckConfiguration
    = CheckConfiguration
    { repoChecks :: [Check]
    , commitChecks :: [Check]
    }
    deriving (Show)

instance FromJSON CheckConfiguration where
    parseJSON = withObject "CheckConfiguration" $ \o -> do
        onRepo <- o .: "on-repository"
        onCommit <- o .: "on-commit"
        return $ CheckConfiguration onRepo onCommit

-- Variables to be expanded in commands
data CheckVariables
    = CheckVariables
    { filename :: FilePath
    , commitRange :: Text
    }

makeFieldLabelsNoPrefix ''CheckVariables

-- Context a check runs in
data CheckContext
    = CheckContext
    { directory :: FilePath
    , commit :: Commit
    , commitRange :: [Commit]
    , variables :: CheckVariables
    }

makeFieldLabelsNoPrefix ''CheckContext
