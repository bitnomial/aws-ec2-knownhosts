{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                ((<=<))
import           Data.Aeson                   (Result (..), fromJSON, json')
import           Data.Bool                    (bool)
import           Data.Function                (on)
import           Data.List                    (foldl')
import           Filesystem.Path.CurrentOS    (encodeString, fromText)
import           Prelude                      hiding (FilePath)
import qualified System.IO.Streams            as Streams
import           System.IO.Streams.Attoparsec (parseFromStream)
import           Turtle                       (ExitCode (..), FilePath, Parser, Text, argPath, argText, exit, format,
                                               fp, isRegularFile, options, proc, rm, stat)

import           AWS.PubKeys                  (writePubKeys)
import           AWS.Types                    (Ec2Instance (..))


main :: IO ()
main = options "Synchronize pubkeys from EC2 to AWS S3" args >>= processPubKeys


args :: Parser (FilePath, FilePath, Text)
args = (,,)
    <$> argPath "local_pubkey_filename" "Local pubkey file to send to S3"
    <*> argPath "remote_pubkey_filename" "Destination pubkey file to send to S3"
    <*> argText "s3_bucket_name" "AWS S3 bucket name to store pubkeys"


processPubKeys :: (FilePath, FilePath, Text) -> IO ()
processPubKeys (localPubKeyFile, remotePubKeyFile, bucketName) = do
    newPubKeys <- readPubKeys localPubKeyFile
    remotePubKeys <- getFromS3 remotePubKeyFile tempRemoteFile bucketName
    writePubKeys (encodeString mergedKeyFile) $ merge remotePubKeys newPubKeys
    copyToS3 mergedKeyFile remotePubKeyFile bucketName
  where
    tempRemoteFile :: FilePath
    tempRemoteFile = "/tmp/" <> fromText bucketName <> "-" <> remotePubKeyFile
    mergedKeyFile = "/tmp/merged-keys.json"


readPubKeys :: FilePath -> IO [Ec2Instance]
readPubKeys pubKeyFile =
    Streams.withFileAsInput (encodeString pubKeyFile) (handleResult <=< parseFromStream pubKeyParser)
  where pubKeyParser = fmap fromJSON json'


merge :: [Ec2Instance] -> [Ec2Instance] -> [Ec2Instance]
merge = foldl' replace
  where
    replace instancePubKeys newInstancePubKey =
        newInstancePubKey : filter (compareFqdn newInstancePubKey) instancePubKeys


compareFqdn :: Ec2Instance -> Ec2Instance -> Bool
compareFqdn = (/=) `on` fqdn


handleResult :: Result a -> IO a
handleResult (Success a) = return a
handleResult (Error s)   = putStrLn ("Parse error: " <> s) >> exit (ExitFailure (-1))


getFromS3 :: FilePath -> FilePath -> Text -> IO [Ec2Instance]
getFromS3 remoteFile tempRemoteFile bucketName = do
    _ <- proc "aws" [ "s3", "cp" , source, format fp tempRemoteFile ] ""
    isRegularFile <$> stat tempRemoteFile >>= bool (return []) (readPubKeys tempRemoteFile)
  where
    source = "s3://" <> bucketName <> "/" <> format fp remoteFile


copyToS3 :: FilePath -> FilePath -> Text -> IO ()
copyToS3 localFile remoteFile bucketName =
    proc "aws" ["s3", "cp", format fp localFile, dest] "" >>= checkUpload
  where
    dest = "s3://" <> bucketName <> "/" <> format fp remoteFile
    checkUpload ExitSuccess = putStrLn "Upload successful"
    checkUpload _ = putStrLn "Failure uploading pubkey file. Check AWS credentials." >> exit (ExitFailure (-1))


cleanup :: FilePath -> FilePath -> IO ()
cleanup tempRemoteFile mergedKeyFile = rm tempRemoteFile >> rm mergedKeyFile
