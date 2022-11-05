{-# LANGUAGE OverloadedStrings #-}

module Main where

import AWS.PubKeys (getPubKeys, instanceParser, writePubKeys)
import AWS.Types (Ec2Instance (..))
import qualified System.IO.Streams as Streams
import System.IO.Streams.Attoparsec (parseFromStream)
import Options.Applicative (Parser)
import qualified Options.Applicative as Opt


main :: IO ()
main = Opt.execParser opts >>= processPubKeys
  where
    opts = Opt.info (Opt.helper <*> args)
      $ Opt.fullDesc
          <> Opt.progDesc "Get SSH pubkeys from EC2 instances as a JSON file"
          <> Opt.header "aws-ec2-pubkeys - Get SSH pubkeys from EC2"


args :: Parser (FilePath, FilePath)
args =
    (,)
        <$> Opt.strArgument (Opt.metavar "INSTANCE_FILE" <> Opt.help "File with information about EC2 hosts")
        <*> Opt.strArgument (Opt.metavar "PUBKEY_FILE" <> Opt.help "Path to JSON pubkey file to output")


-- TODO Implement a function to poll until all pubkeys are available
processPubKeys :: (FilePath, FilePath) -> IO ()
processPubKeys (instanceFile, pubKeyFile) =
    writePubKeys pubKeyFile
        =<< getPubKeys
        =<< readInstances instanceFile


readInstances :: FilePath -> IO [Ec2Instance]
readInstances = ($ parseFromStream instanceParser) . Streams.withFileAsInput
