{-# LANGUAGE OverloadedStrings #-}

module AWS.KnownHosts (
    updateKnownHosts,
) where

import AWS.Types (Ec2Instance (..), Key (..))
import Data.ByteString.Char8 (ByteString)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import GHC.IO.Handle (BufferMode (NoBuffering))
import System.Directory (getHomeDirectory)
import System.FilePath.Posix ((</>))
import System.IO (IOMode (AppendMode))
import qualified System.IO.Streams as Streams
import System.IO.Streams.File (withFileAsOutputExt)
import System.Process (callProcess)
import Prelude hiding (filter, takeWhile)


updateKnownHosts :: [Ec2Instance] -> IO ()
updateKnownHosts ks = do
    mapM_ removeKey ks
    homeDir <- getHomeDirectory
    withFileAsOutputExt (knownHosts homeDir) AppendMode NoBuffering $ writeKeys ks
  where
    knownHosts homeDir = homeDir </> ".ssh" </> "known_hosts"


writeKeys :: [Ec2Instance] -> Streams.OutputStream ByteString -> IO ()
writeKeys ks out = do
    out' <- Streams.intersperse "\n" out
    mapM_ (writeKey out') ks
    Streams.write (Just "\n") out


writeKey :: Streams.OutputStream ByteString -> Ec2Instance -> IO ()
writeKey out inst
    | Just k <- instancePubKey inst =
        Streams.write
            ( Just $
                encodeUtf8
                    ( fqdn inst
                        <> ","
                        <> dns inst
                        <> ","
                        <> instanceId inst
                        <> " "
                        <> keyType k
                        <> " "
                        <> pubKey k
                    )
            )
            out
writeKey _ _ = return ()


removeKey :: Ec2Instance -> IO ()
removeKey inst = mapM_ removeHost [dns inst, fqdn inst, instanceId inst]
  where
    removeHost host = callProcess "ssh-keygen" ["-R", Text.unpack host]
