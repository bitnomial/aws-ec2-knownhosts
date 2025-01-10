{-# LANGUAGE OverloadedStrings #-}

module AWS.PubKeys (
    getPubKeys,
    writePubKeys,
    instanceParser,
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
import Data.Maybe (mapMaybe)
import Data.Text.Encoding (decodeUtf8)
import qualified System.IO.Streams as Streams
import System.IO.Streams.Attoparsec (parseFromStream)
import System.IO.Streams.Process (runInteractiveCommand)
import Text.Printf (printf)
import Prelude hiding (filter, takeWhile)


getPubKeys :: [Ec2Instance] -> IO [Either Ec2Instance Ec2Instance]
getPubKeys = mapConcurrently getPubKey


data GetPubkeyException = NoKeyTypeFound Ec2Instance [Key]
    deriving (Show, Eq)


instance Exception GetPubkeyException


getPubKey :: Ec2Instance -> IO (Either Ec2Instance Ec2Instance)
getPubKey inst = tryJust handler $ do
    (_, stdout, _, _) <- runInteractiveCommand command
    keys' <- Streams.map (filter (/= '\r')) stdout >>= parseFromStream keyParser
    let newInst = (\k -> inst{instancePubKey = Just k}) <$> find ((keytype' ==) . keyType) keys'
    case newInst of
        Just inst' -> pure inst'
        Nothing -> throwIO $ NoKeyTypeFound inst keys'
  where
    handler :: SomeException -> Maybe Ec2Instance
    handler _ = Just inst
    keytype' = "ecdsa-sha2-nistp256"
    command =
        printf
            "aws ec2 get-console-output --region %s --latest --output text --instance-id %s"
            (region inst)
            (instanceId inst)


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
