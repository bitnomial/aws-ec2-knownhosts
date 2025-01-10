{-# LANGUAGE OverloadedStrings #-}

module Main where

import AWS.PubKeys (writePubKeys)
import AWS.Types (Ec2Instance (..))
import Control.Monad ((<=<))
import Data.Aeson (Result (..), fromJSON)
import Data.Aeson.Parser (json')
import Data.Bool (bool)
import Data.Function (on)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Options.Applicative (Parser)
import qualified Options.Applicative as Opt
import System.Directory (removeFile)
import System.Exit (ExitCode (..), exitWith)
import qualified System.IO.Streams as Streams
import System.IO.Streams.Attoparsec (parseFromStream)
import System.PosixCompat (getFileStatus, isRegularFile)
import System.Process (callProcess, system)


main :: IO ()
main = Opt.execParser opts >>= processPubKeys
  where
    opts =
        Opt.info (Opt.helper <*> args) $
            Opt.fullDesc
                <> Opt.progDesc "Copy pubkeys from EC2 to AWS S3"
                <> Opt.header "aws-ec2-keysync - Synchronize pubkeys from EC2 to AWS S3"


args :: Parser (FilePath, FilePath, Text)
args =
    (,,)
        <$> Opt.strArgument (Opt.metavar "LOCAL_PUBKEY_FILENAME" <> Opt.help "Local pubkey file to send to S3")
        <*> Opt.strArgument (Opt.metavar "REMOTE_PUBKEY_FILENAME" <> Opt.help "Destination pubkey file to send to S3")
        <*> Opt.strArgument (Opt.metavar "S3_BUCKET_NAME" <> Opt.help "AWS S3 bucket name to store pubkeys")


processPubKeys :: (FilePath, FilePath, Text) -> IO ()
processPubKeys (localPubKeyFile, remotePubKeyFile, bucketName) = do
    newPubKeys <- readPubKeys localPubKeyFile
    remotePubKeys <- getFromS3 remotePubKeyFile tempRemoteFile bucketName
    writePubKeys mergedKeyFile $ merge remotePubKeys newPubKeys
    copyToS3 mergedKeyFile remotePubKeyFile bucketName
  where
    tempRemoteFile :: FilePath
    tempRemoteFile = "/tmp/" <> Text.unpack bucketName <> "-" <> remotePubKeyFile
    mergedKeyFile = "/tmp/merged-keys.json"


readPubKeys :: FilePath -> IO [Ec2Instance]
readPubKeys pubKeyFile =
    Streams.withFileAsInput
        pubKeyFile
        (handleResult <=< parseFromStream pubKeyParser)
  where
    pubKeyParser = fmap fromJSON json'


merge :: [Ec2Instance] -> [Ec2Instance] -> [Ec2Instance]
merge = foldl' replace
  where
    replace instancePubKeys newInstancePubKey =
        newInstancePubKey : filter (compareFqdn newInstancePubKey) instancePubKeys


compareFqdn :: Ec2Instance -> Ec2Instance -> Bool
compareFqdn = (/=) `on` fqdn


handleResult :: Result a -> IO a
handleResult (Success a) = pure a
handleResult (Error s) = do
    putStrLn ("Parse error: " <> s)
    exitWith (ExitFailure (-1))


getFromS3 :: FilePath -> FilePath -> Text -> IO [Ec2Instance]
getFromS3 remoteFile tempRemoteFile bucketName = do
    _ <- callProcess "aws" ["s3", "cp", source, tempRemoteFile]
    getFileStatus tempRemoteFile >>= bool (return []) (readPubKeys tempRemoteFile) . isRegularFile
  where
    source = "s3://" <> Text.unpack bucketName <> "/" <> remoteFile


copyToS3 :: FilePath -> FilePath -> Text -> IO ()
copyToS3 localFile remoteFile bucketName =
    system (unwords ["aws", "s3", "cp", localFile, dest]) >>= checkUpload
  where
    dest = "s3://" <> Text.unpack bucketName <> "/" <> remoteFile
    checkUpload ExitSuccess = putStrLn "Upload successful"
    checkUpload _ = do
        putStrLn "Failure uploading pubkey file. Check AWS credentials."
        exitWith $ ExitFailure (-1)


cleanup :: FilePath -> FilePath -> IO ()
cleanup tempRemoteFile mergedKeyFile = removeFile tempRemoteFile >> removeFile mergedKeyFile
