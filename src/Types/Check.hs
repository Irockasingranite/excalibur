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

import Data.Aeson
import Data.Yaml as Yaml
import Optics.TH

import Types.Base
import Types.Check.GlobalCheck

-- Implements SPEC-1 @relation(SPEC-1, scope=file)

-- A check to be performed.
data Check
    = CheckGlobalCheck GlobalCheck

instance Show Check where
    show check = case check of
        CheckGlobalCheck c -> show c.name

-- Parse into a specific check variant. Currently trivial.
instance FromJSON Check where
    parseJSON v = CheckGlobalCheck <$> (parseJSON v :: Parser GlobalCheck)

instance ToJSON Check where
    toJSON c = case c of
        CheckGlobalCheck cc -> toJSON cc

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

-- Context a check runs in
data CheckContext
    = CheckContext
    { directory :: FilePath
    , commit :: Commit
    }

makeFieldLabelsNoPrefix ''CheckContext
