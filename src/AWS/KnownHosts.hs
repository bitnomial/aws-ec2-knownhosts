{-# LANGUAGE OverloadedStrings #-}

module AWS.KnownHosts (
    updateKnownHosts,
    knownHostsContent,
) where

import AWS.Types (Ec2Instance (..), Key (..))
import Control.Exception (bracketOnError, finally)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (isJust)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import System.Directory (doesFileExist, removeFile, removePathForcibly, renameFile)
import System.FilePath.Posix (takeDirectory, takeFileName)
import System.IO (hClose, openTempFile)
import System.Process (callProcess)


-- | Replace the entries of each instance that has a key in the known_hosts file
-- at the path. The edit runs on a private copy in the same directory, which is
-- renamed over the file at the end. A reader never sees the file with entries
-- removed, and an interrupted run leaves the file unchanged. Instances without
-- a key are skipped, so their existing entries stay.
updateKnownHosts :: FilePath -> [Ec2Instance] -> IO ()
updateKnownHosts knownHosts instances =
    bracketOnError makeCopy removeFile $ \copy -> do
        mapM_ (removeEntries copy) keyed `finally` removePathForcibly (copy <> ".old")
        remaining <- BS.readFile copy
        BS.writeFile copy $ knownHostsContent remaining instances
        renameFile copy knownHosts
  where
    keyed = filter (isJust . instancePubKey) instances
    makeCopy = do
        (copy, handle) <- openTempFile (takeDirectory knownHosts) (takeFileName knownHosts)
        exists <- doesFileExist knownHosts
        current <- if exists then BS.readFile knownHosts else pure ""
        BS.hPut handle current
        hClose handle
        pure copy


-- | The known_hosts content after the entries of the instances are removed,
-- with a line added for each instance that has a key. Add a newline first when
-- the content does not end in one, so the first new line does not join the last
-- existing one.
knownHostsContent :: ByteString -> [Ec2Instance] -> ByteString
knownHostsContent remaining instances =
    remaining <> separator <> BS.unlines (map knownHostsLine keyed)
  where
    keyed = [(inst, key) | inst <- instances, Just key <- [instancePubKey inst]]
    separator
        | BS.null remaining || BS.last remaining == '\n' = ""
        | otherwise = "\n"


knownHostsLine :: (Ec2Instance, Key) -> ByteString
knownHostsLine (inst, key) =
    encodeUtf8 $
        fqdn inst
            <> ","
            <> dns inst
            <> ","
            <> instanceId inst
            <> " "
            <> keyType key
            <> " "
            <> pubKey key


-- | Remove every line that names the instance's private DNS name, FQDN or
-- instance ID. @ssh-keygen -R@ also matches hashed entries.
removeEntries :: FilePath -> Ec2Instance -> IO ()
removeEntries file inst = mapM_ removeHost [dns inst, fqdn inst, instanceId inst]
  where
    removeHost host = callProcess "ssh-keygen" ["-f", file, "-R", Text.unpack host]
