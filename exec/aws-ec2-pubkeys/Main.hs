{-# LANGUAGE OverloadedStrings #-}

module Main where

import AWS.PubKeys (getPubKeys, instanceParser, writePubKeys)
import AWS.Types (Ec2Instance (..))
import Filesystem.Path.CurrentOS (encodeString)
import qualified System.IO.Streams as Streams
import System.IO.Streams.Attoparsec (parseFromStream)
import Turtle (Parser, argPath, options)


main :: IO ()
main = options "Get SSH pubkeys from EC2" args >>= processPubKeys


args :: Parser (FilePath, FilePath)
args =
    (,)
        <$> (encodeString <$> argPath "instance_file" "File with information about EC2 hosts")
        <*> (encodeString <$> argPath "pubkey_file" "Path to JSON pubkey file to output")


-- TODO Implement a function to poll until all pubkeys are available
processPubKeys :: (FilePath, FilePath) -> IO ()
processPubKeys (instanceFile, pubKeyFile) =
    writePubKeys pubKeyFile
        =<< getPubKeys
        =<< readInstances instanceFile


readInstances :: FilePath -> IO [Ec2Instance]
readInstances = ($ parseFromStream instanceParser) . Streams.withFileAsInput
