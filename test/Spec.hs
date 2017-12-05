{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty
import           Test.Tasty.HUnit

import           Comms.Common.Util
import           Comms.Eth.AddressBook
import           Comms.Eth.Sender
import           Comms.POP3.Server             as POP3
import           Comms.SMTP.Server             as SMTP
import           Comms.Types

import           Control.Concurrent.Async
import           Crypto.Types.PubKey.RSA
import           Network
import           Network.Ethereum.Web3.Address
import           Network.Ethereum.Web3.Types
import           System.IO

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "UnitTests" [cryptoTests, ethTests, serverTests]

ethTests :: TestTree
ethTests = testGroup "Ethereum Tests" [ropstenTests]

cryptoTests :: TestTree
cryptoTests =
  testGroup
    "Cryptography Tests"
    [ testCase "Encrypt/Decrypt" $ do
        let message = "abcdefghijklmnopqrstuvwxyz123456789!@#$%^&*()"
        priv <- genKeyPair
        ciph <- encrypt message (private_pub priv)
        let plain = decrypt ciph priv
        message @?= plain
    ]

ropstenTests :: TestTree
ropstenTests =
  testGroup
    "Ropsten-based Regression Tests"
    [ testCase "Import contact from transaction" $ do
        let txHash =
              "0x7f48e4ad7d16ceb5f2fcae5aaf9e68da3ac957d1c6464ff9f57231dde4593c59" :: TxHash
            inbox = "0x00b258ad4b26daa512209b1c18c8643f10d1a92e" :: Address
        maybeCard <- fetchContactCard txHash
        case maybeCard of
          Left err   -> assertFailure $ show err
          Right card -> inbox @?= inboxAddr card
    ]

serverTests :: TestTree
serverTests = testGroup "Server Tests" [connectionTests]

tearDownServer serverAsync = do
  res <- poll serverAsync
  case res of
    Nothing     -> tearDownServer serverAsync
    Just either -> return ()

connectionTests :: TestTree
connectionTests =
  testGroup
    "Connection Tests"
    [ testCase "Connectable SMTP Socket" $
      -- Setup
       do
        config <- getDefaultConfig
        let smtpPort = fromIntegral $ getSMTPPort config
        runningServer <-
          async $ runServer False config SMTP.handleConn POP3.handleConn
        handle <- connectTo "localhost" (PortNumber $ smtpPort)
      -- Test
        maybeOpenSocket <- hIsOpen handle
        maybeReadableSocket <- hIsReadable handle
        maybeWriteableSocket <- hIsWritable handle
        maybeOpenSocket @?= True
        maybeReadableSocket @?= True
        maybeWriteableSocket @?= True
    -- Cleanup
        cancel runningServer
        tearDownServer runningServer
    , testCase "Connectable POP3 Socket" $
      -- Setup
       do
        config <- getDefaultConfig
        let pop3Port = fromIntegral $ getPOP3Port config
        runningServer <-
          async $ runServer False config SMTP.handleConn POP3.handleConn
        handle <- connectTo "localhost" (PortNumber $ pop3Port)
      -- Test
        maybeOpenSocket <- hIsOpen handle
        maybeReadableSocket <- hIsReadable handle
        maybeWriteableSocket <- hIsWritable handle
        maybeOpenSocket @?= True
        maybeReadableSocket @?= True
        maybeWriteableSocket @?= True
    -- Cleanup
        cancel runningServer
        tearDownServer runningServer
    ]

smtpTests :: TestTree
smtpTests = testGroup "SMTP Tests" []

pop3Tests :: TestTree
pop3Tests = testGroup "POP3 Tests" []
