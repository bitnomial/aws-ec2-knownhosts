{-# LANGUAGE OverloadedStrings #-}

module Main where

import AWS.KnownHosts (knownHostsContent)
import AWS.PubKeys (UploadResult (..), consoleKey, instanceParser, mergePubKeys, syncPubKeys)
import AWS.Types (Ec2Instance (..), Key (..))
import Control.Monad (when)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.Either (isLeft)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))


main :: IO ()
main = defaultMain $ testGroup "aws-ec2-knownhosts" [knownHostsContentTests, mergeTests, syncTests, consoleKeyTests, instanceParserTests]


knownHostsContentTests :: TestTree
knownHostsContentTests =
    testGroup
        "knownHostsContent"
        [ testCase "adds a line after the remaining lines" $
            knownHostsContent otherLine [web "i-new" "AAAAnew"] @?= otherLine <> newLine
        , testCase "adds a newline after a last line that has none" $
            knownHostsContent "other.example.com ssh-ed25519 AAAAother" [web "i-new" "AAAAnew"]
                @?= otherLine <> newLine
        , testCase "adds no line for an instance without a key" $
            knownHostsContent otherLine [(web "i-new" "AAAAnew"){instancePubKey = Nothing}] @?= otherLine
        , testCase "starts an empty file with the new line" $
            knownHostsContent "" [web "i-new" "AAAAnew"] @?= newLine
        ]
  where
    otherLine = "other.example.com ssh-ed25519 AAAAother\n"
    newLine = "web-01.prim.dev.example.com,ip-10-0-0-1,i-new ecdsa-sha2-nistp256 AAAAnew\n"


mergeTests :: TestTree
mergeTests =
    testGroup
        "mergePubKeys"
        [ testCase "replaces a stored entry with the new entry of the same FQDN" $
            mergePubKeys [] [web "i-old" "AAAAold", other] [web "i-new" "AAAAnew"]
                @?= ([web "i-new" "AAAAnew", other], [])
        , testCase "keeps every stored entry without suffixes" $
            mergePubKeys [] [staging] [] @?= ([staging], [])
        , testCase "removes stored entries outside the suffixes" $
            mergePubKeys [".prod.example.com"] [prod, staging] [] @?= ([prod], [staging])
        ]


syncTests :: TestTree
syncTests =
    testGroup
        "syncPubKeys"
        [ testCase "merges again over entries another sync wrote first" $ do
            store <- newIORef (0 :: Int, [other])
            downloads <- newIORef (0 :: Int)
            let download = do
                    count <- atomicModifyIORef' downloads (\n -> (n + 1, n))
                    stored <- readIORef store
                    -- Another sync writes after the first download.
                    when (count == 0) $ modifyIORef' store (\(v, es) -> (v + 1, prod : es))
                    pure (snd stored, fst stored)
                upload version entries =
                    atomicModifyIORef' store $ \(current, es) ->
                        if version == current then ((current + 1, entries), Uploaded) else ((current, es), Conflict)
            result <- syncPubKeys 5 download upload [] [web "i-new" "AAAAnew"]
            final <- snd <$> readIORef store
            (result, final) @?= (Just [], [web "i-new" "AAAAnew", prod, other])
        , testCase "gives up after the attempts all conflict" $ do
            downloads <- newIORef (0 :: Int)
            let download = modifyIORef' downloads (+ 1) >> pure ([], ())
            result <- syncPubKeys 3 download (\_ _ -> pure Conflict) [] [web "i-new" "AAAAnew"]
            count <- readIORef downloads
            (result, count) @?= (Nothing, 3)
        ]


consoleKeyTests :: TestTree
consoleKeyTests =
    testGroup
        "consoleKey"
        [ testCase "finds the ECDSA key in the host key block" $
            consoleKey (consoleOutput "\r\n") @?= Just (Key "ecdsa-sha2-nistp256" "AAAAecdsa")
        , testCase "has no key before the host key block is printed" $
            consoleKey "[    0.000000] Booting Linux\n" @?= Nothing
        ]
  where
    consoleOutput newline =
        mconcat
            [ "[   12.000000] cloud-init: boot"
            , newline
            , "-----BEGIN SSH HOST KEY KEYS-----"
            , newline
            , "ssh-ed25519 AAAAed25519 root@host"
            , newline
            , "ecdsa-sha2-nistp256 AAAAecdsa root@host"
            , newline
            , "-----END SSH HOST KEY KEYS-----"
            , newline
            ]


instanceParserTests :: TestTree
instanceParserTests =
    testGroup
        "instanceParser"
        [ testCase "reads one instance on each line and skips blank lines" $
            fmap (map fqdn) (parseOnly instanceParser (instanceLine <> "\n\n" <> instanceLine <> "\n"))
                @?= Right ["web-01.prim.dev.example.com", "web-01.prim.dev.example.com"]
        , testCase "fails on a line that is not an instance" $
            isLeft (parseOnly instanceParser (instanceLine <> "\n{\"fqdn\": \"x\"}\n")) @?= True
        ]
  where
    instanceLine = "{\"fqdn\": \"web-01.prim.dev.example.com\", \"region\": \"us-east-2\", \"instance_id\": \"i-1\", \"dns\": \"ip-1\"}"


other :: Ec2Instance
other = (web "i-other" "AAAAother"){fqdn = "web-02.prim.dev.example.com"}


prod :: Ec2Instance
prod = (web "i-prod" "AAAAprod"){fqdn = "web-01.prim.prod.example.com"}


staging :: Ec2Instance
staging = (web "i-staging" "AAAAstaging"){fqdn = "web-01.prim.staging.example.com"}


web :: Text -> Text -> Ec2Instance
web instanceId' key =
    Ec2Instance
        { fqdn = "web-01.prim.dev.example.com"
        , region = "us-east-2"
        , instanceId = instanceId'
        , dns = "ip-10-0-0-1"
        , instancePubKey = Just (Key "ecdsa-sha2-nistp256" key)
        }
