{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Comms.Types
import Comms.Common.Util
import Comms.Eth.AddressBook
import Comms.Eth.Sender
    
import Crypto.Types.PubKey.RSA
import Network.Ethereum.Web3.Address
import Network.Ethereum.Web3.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "UnitTests" [cryptoTests, ethTests]

ethTests :: TestTree
ethTests = testGroup "Ethereum Tests" [ropstenTests]

cryptoTests :: TestTree
cryptoTests = testGroup "Cryptography Tests"
 [
  testCase "Encrypt/Decrypt" $ do
    let message = "abcdefghijklmnopqrstuvwxyz123456789!@#$%^&*()"
    priv <- genKeyPair
    ciph <- encrypt message (private_pub priv)
    let plain = decrypt ciph priv
    message @?= plain
 ]

ropstenTests :: TestTree
ropstenTests = testGroup "Ropsten-based Regression Tests"
 [ testCase "Import contact from transaction" $ do
     let txHash = "0x7f48e4ad7d16ceb5f2fcae5aaf9e68da3ac957d1c6464ff9f57231dde4593c59" :: TxHash
         inbox = "0x00b258ad4b26daa512209b1c18c8643f10d1a92e" :: Address
     maybeCard <- fetchContactCard txHash
     case maybeCard of
       Left err -> assertFailure $ show err
       Right card -> inbox @?= inboxAddr card
 ]
