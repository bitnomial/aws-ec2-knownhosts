{-# LANGUAGE OverloadedStrings #-}

module AWS.Types where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value (..),
    object,
    (.:),
    (.:?),
    (.=),
 )
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)


data Key = Key
    { keyType :: Text
    , pubKey :: Text
    }
    deriving (Show, Eq)


data Ec2Instance = Ec2Instance
    { fqdn :: Text
    , region :: Text
    , instanceId :: Text
    , dns :: Text
    , instancePubKey :: Maybe Key
    }
    deriving (Show, Eq)


instance FromJSON Key where
    parseJSON (Object v) =
        Key
            <$> v .: "type"
            <*> v .: "pubkey"
    parseJSON invalid = typeMismatch "Key" invalid


instance ToJSON Key where
    toJSON x =
        object
            [ "type" .= keyType x
            , "pubkey" .= pubKey x
            ]


instance FromJSON Ec2Instance where
    parseJSON (Object v) =
        Ec2Instance
            <$> v .: "fqdn"
            <*> v .: "region"
            <*> v .: "instance_id"
            <*> v .: "dns"
            <*> v .:? "pubkey"
    parseJSON invalid = typeMismatch "Ec2Instance" invalid


instance ToJSON Ec2Instance where
    toJSON x =
        object
            [ "fqdn" .= fqdn x
            , "region" .= region x
            , "instance_id" .= instanceId x
            , "dns" .= dns x
            , "pubkey" .= instancePubKey x
            ]
