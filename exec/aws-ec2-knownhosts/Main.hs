{-# LANGUAGE OverloadedStrings #-}

module Main where

import AWS.KnownHosts (updateKnownHosts)
import AWS.PubKeys (inScope)
import AWS.Types (Ec2Instance (..))
import Data.Aeson (Result (..), fromJSON)
import Data.Aeson.Parser (json)
import Data.Text (Text)
import Options.Applicative (Parser)
import qualified Options.Applicative as Opt
import System.Directory (getHomeDirectory)
import System.Exit (die)
import System.FilePath.Posix ((</>))
import qualified System.IO.Streams as Streams
import System.IO.Streams.Attoparsec (parseFromStream)


main :: IO ()
main = do
    (suffixes, pubKeyFile) <- Opt.execParser opts
    instances <- readKeys pubKeyFile
    homeDir <- getHomeDirectory
    updateKnownHosts (homeDir </> ".ssh" </> "known_hosts") (filter (inScope suffixes) instances)
  where
    opts =
        Opt.info (Opt.helper <*> args) $
            Opt.fullDesc
                <> Opt.progDesc "Update known_hosts file with pubkeys from EC2"
                <> Opt.header "aws-ec2-knownhosts - Get EC2 pubkeys on your known_hosts file"


args :: Parser ([Text], FilePath)
args =
    (,)
        <$> Opt.many
            ( Opt.strOption $
                Opt.long "fqdn-suffix"
                    <> Opt.metavar "SUFFIX"
                    <> Opt.help "Load only entries whose FQDN ends in SUFFIX. Repeat for more suffixes."
            )
        <*> Opt.strArgument (Opt.metavar "PUBKEY_FILE" <> Opt.help "Path to JSON pubkey file to update knownhosts")


-- | Exit non-zero when the file does not match the schema, so a deploy does not
-- continue with an unchanged known_hosts file.
readKeys :: FilePath -> IO [Ec2Instance]
readKeys file = do
    value <- Streams.withFileAsInput file (parseFromStream json)
    case fromJSON value of
        Success instances -> pure instances
        Error message -> die $ "Cannot read pubkey file " <> file <> ": " <> message
