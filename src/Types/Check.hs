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

module Types.Check where

import Data.Aeson
import Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Data.Yaml as Yaml
import Optics.TH

import Types.Base
import Types.Check.CommandCheck

-- A check to be performed.
data Check
    = CheckCommandCheck CommandCheck
    deriving (Show)

instance FromJSON Check where
    parseJSON =
        withObject
            "Check"
            $ \o -> do
                mBuiltin <- o .:? "builtin" :: Parser (Maybe String)
                case mBuiltin of
                    Nothing -> CheckCommandCheck <$> parseJSON (Object o)
                    _ -> fail "Unknown builtin check"

instance ToJSON Check where
    toJSON c = case c of
        CheckCommandCheck cc -> toJSON cc

-- A check with an attached name.
data NamedCheck
    = NamedCheck
    { name :: Text
    , inner :: Check
    }
    deriving (Show)

instance FromJSON NamedCheck where
    parseJSON =
        withObject "NamedCheck" $ \v -> do
            name_ <- v .: "name"
            check <- parseJSON (Object v)
            return $ NamedCheck name_ check

instance ToJSON NamedCheck where
    toJSON nc =
        let oCheck = toJSON nc.inner
            outer = KM.fromList ["name" .= nc.name]
         in case oCheck of
                Object check -> Object (outer `union` check)
                _ -> error "Invalid check encoding"

-- Implements SPEC-1 @relation(SPEC-1, scope=range_start)
-- A complete check configuration.
data CheckConfiguration
    = CheckConfiguration
    { globalChecks :: [NamedCheck]
    , perCommitChecks :: [NamedCheck]
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
