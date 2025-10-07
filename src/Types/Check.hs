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
import Types.Check.CommandCheck

-- A check to be performed.
data Check
    = CheckCommandCheck CommandCheck

instance Show Check where
    show check = case check of
        CheckCommandCheck c -> show c.name

-- Parse into a specific check variant. Currently trivial.
instance FromJSON Check where
    parseJSON v = CheckCommandCheck <$> (parseJSON v :: Parser CommandCheck)

instance ToJSON Check where
    toJSON c = case c of
        CheckCommandCheck cc -> toJSON cc

-- Implements SPEC-1 @relation(SPEC-1, scope=range_start)
-- A complete check configuration.
data CheckConfiguration
    = CheckConfiguration
    { globalChecks :: [Check]
    , perCommitChecks :: [Check]
    }
    deriving (Show)

instance FromJSON CheckConfiguration where
    parseJSON = withObject "CheckConfiguration" $ \o -> do
        global <- o .: "global"
        perCommit <- o .: "per_commit"
        return $ CheckConfiguration global perCommit

-- @relation(SPEC-1, scope=range_end)

-- Context a check runs in
data CheckContext
    = CheckContext
    { directory :: FilePath
    , commit :: Commit
    }

makeFieldLabelsNoPrefix ''CheckContext
