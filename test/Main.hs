{-# LANGUAGE OverloadedStrings #-}

module Main where

import AWS.KnownHosts (knownHostsContent)
import AWS.Types (Ec2Instance (..), Key (..))
import Data.Text (Text)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))


main :: IO ()
main = defaultMain $ testGroup "aws-ec2-knownhosts" [knownHostsContentTests]


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


web :: Text -> Text -> Ec2Instance
web instanceId' key =
    Ec2Instance
        { fqdn = "web-01.prim.dev.example.com"
        , region = "us-east-2"
        , instanceId = instanceId'
        , dns = "ip-10-0-0-1"
        , instancePubKey = Just (Key "ecdsa-sha2-nistp256" key)
        }
