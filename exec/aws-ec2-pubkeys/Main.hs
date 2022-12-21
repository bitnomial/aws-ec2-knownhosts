{-# LANGUAGE OverloadedStrings #-}

module Main where

import AWS.PubKeys (getPubKeys, instanceParser, writePubKeys)
import AWS.Types (Ec2Instance (..))
import Data.Either (partitionEithers)
import qualified Data.Text.IO as T
import Options.Applicative (Parser)
import qualified Options.Applicative as Opt
import System.Exit (exitFailure)
import qualified System.IO.Streams as Streams
import System.IO.Streams.Attoparsec (parseFromStream)


main :: IO ()
main = Opt.execParser opts >>= processPubKeys
  where
    opts =
        Opt.info (Opt.helper <*> args) $
            Opt.fullDesc
                <> Opt.progDesc "Get SSH pubkeys from EC2 instances as a JSON file"
                <> Opt.header "aws-ec2-pubkeys - Get SSH pubkeys from EC2"


args :: Parser (FilePath, FilePath)
args =
    (,)
        <$> Opt.strArgument (Opt.metavar "INSTANCE_FILE" <> Opt.help "File with information about EC2 hosts")
        <*> Opt.strArgument (Opt.metavar "PUBKEY_FILE" <> Opt.help "Path to JSON pubkey file to output")


-- TODO Implement a function to poll until all pubkeys are available
processPubKeys :: (FilePath, FilePath) -> IO ()
processPubKeys (instanceFile, pubKeyFile) = do
    insts <- readInstances instanceFile
    T.putStrLn "Scanning these instances:"
    printFqdns insts
    (failedInsts, keyedInsts) <- partitionEithers <$> getPubKeys insts
    if null failedInsts
        then writePubKeys pubKeyFile keyedInsts
        else do
            T.putStrLn "Still waiting on the following instances:"
            printFqdns failedInsts
            exitFailure
  where
    printFqdns = mapM_ (T.putStrLn . fqdn)


readInstances :: FilePath -> IO [Ec2Instance]
readInstances = ($ parseFromStream instanceParser) . Streams.withFileAsInput
