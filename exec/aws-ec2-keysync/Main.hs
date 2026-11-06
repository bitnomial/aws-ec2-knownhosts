{-# LANGUAGE OverloadedStrings #-}

module Main where

import AWS.PubKeys (UploadResult (..), inScope, syncPubKeys, writePubKeys)
import AWS.Types (Ec2Instance (..))
import Control.Monad ((<=<))
import Data.Aeson (Result (..), eitherDecode, fromJSON, withObject, (.:))
import Data.Aeson.Parser (json')
import Data.Aeson.Types (parseEither)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy
import Options.Applicative (Parser)
import qualified Options.Applicative as Opt
import System.Exit (ExitCode (..), die, exitWith)
import System.IO (stderr)
import System.IO.Extra (withTempFile)
import qualified System.IO.Streams as Streams
import System.IO.Streams.Attoparsec (parseFromStream)
import System.Process (readProcessWithExitCode)


main :: IO ()
main = Opt.execParser opts >>= processPubKeys
  where
    opts =
        Opt.info (Opt.helper <*> args) $
            Opt.fullDesc
                <> Opt.progDesc "Copy pubkeys from EC2 to AWS S3"
                <> Opt.header "aws-ec2-keysync - Synchronize pubkeys from EC2 to AWS S3"


data Args = Args
    { fqdnSuffixes :: [Text]
    , localPubKeyFile :: FilePath
    , remotePubKeyFile :: FilePath
    , bucketName :: Text
    }


args :: Parser Args
args =
    Args
        <$> Opt.many
            ( Opt.strOption $
                Opt.long "fqdn-suffix"
                    <> Opt.metavar "SUFFIX"
                    <> Opt.help
                        "Keep only entries whose FQDN ends in SUFFIX. Repeat for more suffixes. \
                        \Refuses local entries outside them and removes stored entries outside them."
            )
        <*> Opt.strArgument (Opt.metavar "LOCAL_PUBKEY_FILENAME" <> Opt.help "Local pubkey file to send to S3")
        <*> Opt.strArgument (Opt.metavar "REMOTE_PUBKEY_FILENAME" <> Opt.help "Destination pubkey file to send to S3")
        <*> Opt.strArgument (Opt.metavar "S3_BUCKET_NAME" <> Opt.help "AWS S3 bucket name to store pubkeys")


processPubKeys :: Args -> IO ()
processPubKeys options = do
    newPubKeys <- readPubKeys (localPubKeyFile options)
    case filter (not . inScope (fqdnSuffixes options)) newPubKeys of
        [] -> pure ()
        outside -> die $ "Refusing entries outside --fqdn-suffix: " <> unwords (map (Text.unpack . fqdn) outside)
    result <-
        withTempFile $ \tempRemoteFile ->
            withTempFile $ \mergedKeyFile ->
                syncPubKeys
                    maxAttempts
                    (download object tempRemoteFile)
                    (upload object mergedKeyFile)
                    (fqdnSuffixes options)
                    newPubKeys
    case result of
        Just removed -> do
            mapM_ (Text.hPutStrLn stderr . ("Removed entry outside --fqdn-suffix: " <>) . fqdn) removed
            putStrLn "Upload successful"
        Nothing -> die "The pubkey file kept changing during the sync. Try again."
  where
    object = S3Object (bucketName options) (remotePubKeyFile options)
    maxAttempts = 5


readPubKeys :: FilePath -> IO [Ec2Instance]
readPubKeys pubKeyFile =
    Streams.withFileAsInput
        pubKeyFile
        (handleResult <=< parseFromStream pubKeyParser)
  where
    pubKeyParser = fmap fromJSON json'


handleResult :: Result a -> IO a
handleResult (Success a) = pure a
handleResult (Error s) = do
    putStrLn ("Parse error: " <> s)
    exitWith (ExitFailure (-1))


data S3Object = S3Object
    { objectBucket :: Text
    , objectKey :: FilePath
    }


s3Args :: String -> S3Object -> [String]
s3Args command object = ["s3api", command, "--bucket", Text.unpack (objectBucket object), "--key", objectKey object]


-- | Download the stored entries and the object's ETag. A missing object has no
-- entries and no ETag.
download :: S3Object -> FilePath -> IO ([Ec2Instance], Maybe Text)
download object file = do
    (code, out, err) <- readProcessWithExitCode "aws" (s3Args "get-object" object <> [file]) ""
    case code of
        ExitSuccess -> do
            entries <- readPubKeys file
            etag <- either (die . ("Cannot read the ETag of the pubkey file: " <>)) pure $ responseETag out
            pure (entries, Just etag)
        ExitFailure _
            | "NoSuchKey" `isInfixOf` err -> pure ([], Nothing)
            | otherwise -> die $ "Failure downloading pubkey file. Check AWS credentials.\n" <> err
  where
    responseETag = parseEither (withObject "GetObjectOutput" (.: "ETag")) <=< eitherDecode . Text.Lazy.encodeUtf8 . Text.Lazy.pack


-- | Write the entries and upload them only if the object is unchanged since the
-- download, so a concurrent sync cannot be overwritten. With no ETag, upload
-- only if the object is still missing.
upload :: S3Object -> FilePath -> Maybe Text -> [Ec2Instance] -> IO UploadResult
upload object file etag entries = do
    writePubKeys file entries
    (code, _, err) <- readProcessWithExitCode "aws" (s3Args "put-object" object <> body <> condition) ""
    case code of
        ExitSuccess -> pure Uploaded
        ExitFailure _
            | any (`isInfixOf` err) ["PreconditionFailed", "ConditionalRequestConflict"] -> do
                putStrLn "The pubkey file changed during the sync. Merging again."
                pure Conflict
            | otherwise -> die $ "Failure uploading pubkey file. Check AWS credentials.\n" <> err
  where
    body = ["--body", file, "--content-type", "application/json"]
    condition = maybe ["--if-none-match", "*"] (\tag -> ["--if-match", Text.unpack tag]) etag
