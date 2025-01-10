{-# LANGUAGE OverloadedStrings #-}

module Main where

import AWS.KnownHosts (updateKnownHosts)
import AWS.Types (Ec2Instance (..))
import Data.Aeson (Result (..), fromJSON)
import Data.Aeson.Parser (json)
import Options.Applicative (Parser)
import qualified Options.Applicative as Opt
import qualified System.IO.Streams as Streams
import System.IO.Streams.Attoparsec (parseFromStream)


main :: IO ()
main = do
    Opt.execParser opts
        >>= readKeys
        >>= maybe noKeys updateKnownHosts
  where
    opts =
        Opt.info (Opt.helper <*> args) $
            Opt.fullDesc
                <> Opt.progDesc "Update known_hosts file with pubkeys from EC2"
                <> Opt.header "aws-ec2-knownhosts - Get EC2 pubkeys on your known_hosts file"


args :: Parser FilePath
args =
    Opt.strArgument $
        Opt.metavar "PUBKEY_FILE" <> Opt.help "Path to JSON pubkey file to update knownhosts"


readKeys :: FilePath -> IO (Maybe [Ec2Instance])
readKeys =
    fmap (maybeResult . fromJSON)
        . flip Streams.withFileAsInput (parseFromStream json)


maybeResult :: Result a -> Maybe a
maybeResult (Success x) = Just x
maybeResult (Error _) = Nothing


noKeys :: IO ()
noKeys = putStrLn "No keys were found in that pub_key_file"
