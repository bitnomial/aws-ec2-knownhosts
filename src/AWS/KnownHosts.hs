{-# LANGUAGE OverloadedStrings #-}

module AWS.KnownHosts
  ( updateKnownHosts
  ) where

import           Data.ByteString.Char8     (ByteString)
import           Data.Monoid               ((<>))
import           Data.Text.Encoding        (encodeUtf8)
import qualified Data.Text.Lazy            as LT
import           GHC.IO.Handle             (BufferMode (NoBuffering))
import           Prelude                   hiding (filter, takeWhile)
import           System.Directory          (getHomeDirectory)
import           System.Exit               (ExitCode)
import           System.FilePath.Posix     ((</>))
import           System.IO                 (IOMode (AppendMode))
import qualified System.IO.Streams         as Streams
import           System.IO.Streams.File    (withFileAsOutputExt)
import           System.IO.Streams.Process (runInteractiveCommand)
import           System.Process            (waitForProcess)
import           Text.Printf               (printf)

import           AWS.Types                 (Ec2Instance (..), Key (..))


updateKnownHosts :: [Ec2Instance] -> IO ()
updateKnownHosts ks = do
    homeDir <- getHomeDirectory
    withFileAsOutputExt (knownHosts homeDir) AppendMode NoBuffering $ updateKeys ks
  where
    knownHosts homeDir = homeDir </> ".ssh" </> "known_hosts"


updateKeys :: [Ec2Instance] -> Streams.OutputStream ByteString -> IO ()
updateKeys ks out = do
    out' <- Streams.intersperse "\n" out
    mapM_ (updateKey out') ks
    Streams.write (Just "\n") out


updateKey :: Streams.OutputStream ByteString -> Ec2Instance -> IO ()
updateKey out inst | Just k <- instancePubKey inst =
    removeKey inst `seq`
    Streams.write (Just $ encodeUtf8 (fqdn inst <> "," <> dns inst <> " " <> keyType k <> " " <> pubKey k)) out
updateKey _ _ = return ()


removeKey :: Ec2Instance -> IO ExitCode
removeKey inst = do
    (_,_,_,pid1) <- runInteractiveCommand $ printf removeKeyCommand (dns inst)
    waitForProcess pid1
    (_,_,_,pid2) <- runInteractiveCommand $ printf removeKeyCommand (fqdn inst)
    waitForProcess pid2
  where
    removeKeyCommand = "ssh-keygen -R %s"
