{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AWS.PubKeys (
    getPubKeys,
    writePubKeys,
    instanceParser,
    inScope,
    mergePubKeys,
    UploadResult (..),
    syncPubKeys,
) where

import AWS.Types (Ec2Instance (..), Key (..))
import Control.Applicative (Alternative, (<|>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (Exception, SomeException, throwIO, tryJust)
import Data.Aeson (decodeStrict', encode)
import qualified Data.Attoparsec.ByteString as W8
import Data.Attoparsec.ByteString.Char8 (
    Parser,
    endOfInput,
    endOfLine,
    isEndOfLine,
    isSpace,
    manyTill',
    skipSpace,
    string,
    (<?>),
 )
import qualified Data.Attoparsec.ByteString.Char8 as C8
import Data.ByteString.Char8 (ByteString, filter)
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.List (find)
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified System.IO.Streams as Streams
import System.IO.Streams.Attoparsec (parseFromStream)
import System.IO.Streams.Process (runInteractiveProcess)
import Prelude hiding (filter, takeWhile)


getPubKeys :: [Ec2Instance] -> IO [Either Ec2Instance Ec2Instance]
getPubKeys = mapConcurrently getPubKey


data GetPubkeyException = NoKeyTypeFound Ec2Instance [Key]
    deriving (Show, Eq)


instance Exception GetPubkeyException


getPubKey :: Ec2Instance -> IO (Either Ec2Instance Ec2Instance)
getPubKey inst = tryJust handler $ do
    (_, stdout, _, _) <- runInteractiveProcess "aws" awsArgs Nothing Nothing
    keys' <- Streams.map (filter (/= '\r')) stdout >>= parseFromStream keyParser
    let newInst = (\k -> inst{instancePubKey = Just k}) <$> find ((keytype' ==) . keyType) keys'
    case newInst of
        Just inst' -> pure inst'
        Nothing -> throwIO $ NoKeyTypeFound inst keys'
  where
    handler :: SomeException -> Maybe Ec2Instance
    handler _ = Just inst
    keytype' = "ecdsa-sha2-nistp256"
    awsArgs =
        [ "ec2"
        , "get-console-output"
        , "--region"
        , Text.unpack (region inst)
        , "--latest"
        , "--output"
        , "text"
        , "--instance-id"
        , Text.unpack (instanceId inst)
        ]


-- | Whether the instance's FQDN ends in one of the suffixes. An empty list
-- matches every instance.
inScope :: [Text] -> Ec2Instance -> Bool
inScope [] _ = True
inScope suffixes inst = any (`Text.isSuffixOf` fqdn inst) suffixes


-- | Merge new entries into stored ones by FQDN, and the new entry wins. Stored
-- entries outside the scope are removed, so a pubkey file holds only its own
-- stage. Return the merged entries and the removed ones.
mergePubKeys :: [Text] -> [Ec2Instance] -> [Ec2Instance] -> ([Ec2Instance], [Ec2Instance])
mergePubKeys suffixes stored new = (List.foldl' replace kept new, removed)
  where
    (kept, removed) = List.partition (inScope suffixes) stored
    replace entries entry = entry : List.filter ((/= fqdn entry) . fqdn) entries


data UploadResult = Uploaded | Conflict
    deriving (Show, Eq)


-- | Download the stored entries and their version, merge the new entries in,
-- and upload the result on the condition that the version is unchanged. On a
-- 'Conflict' another sync wrote first, so download and merge again, up to the
-- number of attempts. Return the stored entries removed by the scope, or
-- 'Nothing' when every attempt conflicted.
syncPubKeys ::
    (Monad m) =>
    Int ->
    m ([Ec2Instance], version) ->
    (version -> [Ec2Instance] -> m UploadResult) ->
    [Text] ->
    [Ec2Instance] ->
    m (Maybe [Ec2Instance])
syncPubKeys attempts download upload suffixes new
    | attempts < 1 = pure Nothing
    | otherwise = do
        (stored, version) <- download
        let (merged, removed) = mergePubKeys suffixes stored new
        upload version merged >>= \case
            Uploaded -> pure (Just removed)
            Conflict -> syncPubKeys (attempts - 1) download upload suffixes new


writePubKeys :: FilePath -> [Ec2Instance] -> IO ()
writePubKeys file = Streams.withFileAsOutput file . Streams.write . Just . toStrict . encode


instanceParser :: Parser [Ec2Instance]
instanceParser = mapMaybe decodeStrict' <$> line <?> "instances"
  where
    line = W8.takeTill isEndOfLine `W8.sepBy` endOfLine <* endOfInput


keyParser :: Parser [Key]
keyParser = skipToKeys *> keys <?> "keyParser"


skipToKeys :: Parser ()
skipToKeys = skipLine `skipTill` keysBegin <?> "skipToKeys"
  where
    skipTill :: (Alternative f) => f a -> f b -> f b
    skipTill s n = n <|> s *> skipTill s n


skipLine :: Parser ()
skipLine = W8.skipWhile (not . isEndOfLine) <* endOfLine <?> "skip line"


exactLine :: ByteString -> Parser ()
exactLine s = string s *> endOfLine <?> "exact line"


keysBegin :: Parser ()
keysBegin = exactLine "-----BEGIN SSH HOST KEY KEYS-----" <?> "keys begin"


keysEnd :: Parser ()
keysEnd = exactLine "-----END SSH HOST KEY KEYS-----" <?> "keys end"


keys :: Parser [Key]
keys = manyTill' key keysEnd <?> "keys"


key :: Parser Key
key =
    do
        keyType' <- decodeUtf8 <$> C8.takeTill isSpace
        skipSpace
        pubKey' <- decodeUtf8 <$> C8.takeTill isSpace
        skipLine
        return $ Key keyType' pubKey'
        <?> "key"
