{-# LANGUAGE OverloadedStrings #-}

module AWS.KnownHosts (
    updateKnownHosts,
) where

import AWS.Types (Ec2Instance (..), Key (..))
import Data.ByteString.Char8 (ByteString)
import Data.Text.Encoding (encodeUtf8)
import GHC.IO.Handle (BufferMode (NoBuffering))
import System.Directory (getHomeDirectory)
import System.Exit (ExitCode)
import System.FilePath.Posix ((</>))
import System.IO (IOMode (AppendMode))
import qualified System.IO.Streams as Streams
import System.IO.Streams.File (withFileAsOutputExt)
import System.IO.Streams.Process (runInteractiveCommand)
import System.Process (waitForProcess)
import Text.Printf (printf)
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


removeKey :: Ec2Instance -> IO ExitCode
removeKey inst = do
    (_, _, _, pid1) <- runInteractiveCommand $ printf removeKeyCommand (dns inst)
    _ <- waitForProcess pid1
    (_, _, _, pid2) <- runInteractiveCommand $ printf removeKeyCommand (fqdn inst)
    _ <- waitForProcess pid2
    (_, _, _, pid3) <- runInteractiveCommand $ printf removeKeyCommand (instanceId inst)
    waitForProcess pid3
  where
    removeKeyCommand = "ssh-keygen -R %s"
