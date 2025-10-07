{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Check where

import Control.Lens (makeLenses, makePrisms, (^.))
import Data.Aeson
import Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Data.Yaml as Yaml

import Types.Base
import Types.Check.CommandCheck

-- A check to be performed.
data Check
    = CheckCommandCheck CommandCheck
    deriving (Show)

makePrisms ''Check

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
    { _checkName :: Text
    , _checkInner :: Check
    }
    deriving (Show)

makeLenses ''NamedCheck

instance FromJSON NamedCheck where
    parseJSON =
        withObject "NamedCheck" $ \v -> do
            name <- v .: "name"
            check <- parseJSON (Object v)
            return $ NamedCheck name check

instance ToJSON NamedCheck where
    toJSON nc =
        let oCheck = toJSON (nc ^. checkInner)
            outer = KM.fromList ["name" .= (nc ^. checkName)]
         in case oCheck of
                Object check -> Object (outer `union` check)
                _ -> error "Invalid check encoding"

-- A complete check configuration.
data CheckConfiguration
    = CheckConfiguration
    { _globalChecks :: [NamedCheck]
    , _perCommitChecks :: [NamedCheck]
    }
    deriving (Show)

makeLenses ''CheckConfiguration

-- Implements SPEC-1 @relation(SPEC-1, scope=range_start)
instance FromJSON CheckConfiguration where
    parseJSON = withObject "CheckConfiguration" $ \o -> do
        global <- o .: "global"
        perCommit <- o .: "per_commit"
        return $ CheckConfiguration global perCommit

-- @relation(SPEC-1, scope=range_end)

-- Context a check runs in
data CheckContext
    = CheckContext
    { _contextDirectory :: FilePath
    , _contextCommit :: Commit
    }

makeLenses ''CheckContext
