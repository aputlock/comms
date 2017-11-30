{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Comms.Eth.Sender where

import           Comms.Common.Types
import           Comms.Common.Util
import           Comms.Eth.Cost
import           Comms.Eth.Provider

import           Crypto.Types.PubKey.RSA

import           Control.Monad.IO.Class

import           Data.Aeson
import           Data.Aeson.Text
import           Data.Aeson.Types
import           Data.Bits
import           Data.Char
import           Data.Either
import           Data.Maybe
import qualified Data.ByteArray                as BA
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Builder        as TLB
import qualified Data.Text.Read                as TR
    
import           Network.Ethereum.Web3
import           Network.Ethereum.Web3.Eth
import           Network.Ethereum.Web3.TH
import           Network.Ethereum.Web3.Types

[abiFrom|solidity/contract.json|]

bytesDecode :: T.Text -> Bytes
{-# INLINE bytesDecode #-}
bytesDecode = BA.convert . T.encodeUtf8

-- Creates a transaction with the user's ContactCard
sendContactCard :: IO (Either Web3Error TxHash)
sendContactCard = do
  cfg <- getDefaultConfig
  priv <- getKeyPair defaultKeyFile
  contract <- getContract contractFile
  call <- getCall contract "registerUser"
  let contactCard = ContactCard { inboxAddr = walletAddr cfg, publicKey = private_pub priv }
      builder = encodeToTextBuilder contactCard
      lazy = TLB.toLazyText builder
      bytes = bytesDecode $ TL.toStrict lazy
  runUser $ registerUser contractAddr nopay (BytesD bytes)

-- Extracts the Text corresponding to the ContactCard
getContactCard :: Text -> IO Text
getContactCard txt = do
  contract <- getContract contractFile
  let method = methodHash "registerUser" contract
      args = fromJust $ T.stripPrefix method txt -- drop method hash
      offset = 2 * (fst $ fromRight $ TR.hexadecimal (T.take 64 args))
      len = 2 * (fst $ fromRight $ TR.hexadecimal $ T.take 64 (T.drop offset args))
  if T.take 10 txt /= method then error "Invalid contact card"
  else return $ T.take len $ T.drop 128 args

-- Imports contact at hash into address book with given email
importContact :: String -> TxHash -> IO ()
importContact email hash = do
  card <- fetchContactCard hash
  contact <- lookupContact email
  case contact of
    Just _ -> error $ "Contact already exists with email address: " ++ email
    Nothing -> addContact contactFile $ Contact email hash card

-- Fetches the ContactCard at the given transaction hash
fetchContactCard :: TxHash -> IO ContactCard
fetchContactCard hash = do
  eith <- runUser $ getTransactionByHash hash
  let serial = txInput . fromJust . fromRight $ eith
  contact <- getContactCard serial
  return $ fromRight ((eitherDecode . BSL.fromStrict . T.encodeUtf8 . hexToAscii $ contact) :: Either String ContactCard)

-- Sends a message to the given user
sendEmail :: String -> String -> IO (Either Web3Error TxHash)
sendEmail usr msg = do
  recip <- lookupContact usr
  case recip of
    Nothing -> return $ Left $ UserFail $ "No contact with email address: " ++ usr
    Just r -> do
               ciph <- encrypt msg $ publicKey r
               runUser $ sendMessage contractAddr nopay (inboxAddr r) (BytesD $ bytesDecode $ T.pack ciph) 
