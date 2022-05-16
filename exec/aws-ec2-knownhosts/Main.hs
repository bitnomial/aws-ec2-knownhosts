{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson                   (Result (..), fromJSON, json)
import           Filesystem.Path.CurrentOS    (encodeString)
import           Prelude                      hiding (FilePath)
import qualified System.IO.Streams            as Streams
import           System.IO.Streams.Attoparsec (parseFromStream)
import           Turtle                       (FilePath, Parser, argPath, options)

import           AWS.KnownHosts               (updateKnownHosts)
import           AWS.Types                    (Ec2Instance (..))


main :: IO ()
main = options "Update known_hosts file with pubkeys from EC2" args >>= readKeys >>= maybe noKeys updateKnownHosts


args :: Parser FilePath
args = argPath "pubkey_file" "Path to JSON pubkey file to update knownhosts"


readKeys :: FilePath -> IO (Maybe [Ec2Instance])
readKeys = fmap (maybeResult . fromJSON) . flip Streams.withFileAsInput (parseFromStream json) . encodeString


maybeResult :: Result a -> Maybe a
maybeResult (Success x) = Just x
maybeResult (Error _)   = Nothing


noKeys :: IO ()
noKeys = putStrLn "No keys were found in that pub_key_file"
