{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Comms.Eth.Sender where

import           Comms.Types
import           Comms.Common.Util
import           Comms.Eth.AddressBook
import           Comms.Eth.Cost
import           Comms.Eth.Provider

import           Crypto.Types.PubKey.RSA

import qualified Data.Aeson.Text               as AT
import qualified Data.ByteArray                as BA
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Builder        as TLB

import           Network.Ethereum.Web3.Address
import           Network.Ethereum.Web3.Contract
import           Network.Ethereum.Web3.Encoding.Bytes
import           Network.Ethereum.Web3.TH
import           Network.Ethereum.Web3.Types

[abiFrom|solidity/contract.json|]

bytesDecode :: T.Text -> BA.Bytes
{-# INLINE bytesDecode #-}
bytesDecode = BA.convert . T.encodeUtf8


-- | Sends a message to the given user
sendEmail :: String -> String -> IO (Either Web3Error TxHash)
sendEmail usr msg = do
  recip <- lookupContact usr
  case recip of
    Nothing -> return $ Left $ UserFail $ "No contact with email address: " ++ usr
    Just r -> do
               ciph <- encrypt msg $ publicKey r
               runUser $ sendMessage contractAddr nopay (inboxAddr r) (BytesD $ bytesDecode $ T.pack ciph) 


{- | Creates a transaction with the user's ContactCard -}
sendContactCard :: IO (Either Web3Error TxHash)
sendContactCard = do
  cfg <- getDefaultConfig
  priv <- getKeyPair defaultKeyFile
  let contract = getContract contractFile
      contactCard = ContactCard { inboxAddr = walletAddr cfg, publicKey = private_pub priv }
      builder = AT.encodeToTextBuilder contactCard
      lazy = TLB.toLazyText builder
      bytes = bytesDecode $ TL.toStrict lazy
  runUser $ registerUser contractAddr nopay (BytesD bytes)
