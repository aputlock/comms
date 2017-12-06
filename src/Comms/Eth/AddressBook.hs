{-# LANGUAGE OverloadedStrings #-}

module Comms.Eth.AddressBook where

import           Comms.Types
import           Comms.Common.Util
import           Comms.Eth.Cost
import           Comms.Eth.Provider

import           Data.Aeson
import           Data.Aeson.Text
import qualified Data.ByteArray                as BA
import qualified Data.ByteString.Lazy          as BSL
import           Data.Either
import           Data.List
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Builder        as TLB
import qualified Data.Text.Read                as TR

import           Network.Ethereum.Web3.Eth
import           Network.Ethereum.Web3.Types

import           System.Directory

-- Fetches the address book at the given path
getContacts :: FilePath -> IO [Contact]
getContacts path = do
  exists <- doesFileExist path
  if (not exists) then return []
  else do
    bytes <- BSL.readFile path
    let d =  eitherDecode bytes :: Either String [Contact]
    case d of
      Left err  -> error $ "bad contacts: " ++ err
      Right cfg -> return cfg

-- Fetches the ContactCard associated with the given email address
lookupContact :: String -> IO (Maybe ContactCard)
lookupContact email = do
  contacts <- getContacts =<< contactFile
  return $ case find (\x -> emailAddr x == email) contacts of
             Nothing -> Nothing
             Just contact -> Just $ contactInfo contact

-- Adds a new contact to the address book at the given path
addContact :: FilePath -> Contact -> IO ()
addContact path contact = do
  contacts <- getContacts path
  BSL.writeFile path $ encode $ contact:contacts

-- Extracts the Text corresponding to the ContactCard
parseContactCard :: T.Text -> IO (Either Web3Error ContactCard)
parseContactCard txt = do
  let contract = getContract contractFile
      method = methodHash "registerUser" contract
      args = fromJust $ T.stripPrefix method txt 
      offset = 2 * (fst $ fromRight $ TR.hexadecimal (T.take 64 args))
      len = 2 * (fst $ fromRight $ TR.hexadecimal $ T.take 64 (T.drop offset args))
      contact = T.take len $ T.drop 128 args
      maybeCard = ((eitherDecode . BSL.fromStrict . T.encodeUtf8 . hexToAscii $ contact) :: Either String ContactCard)
  if T.take 10 txt /= method then error "Invalid contact card"
  else case maybeCard of
         Left err -> return $ Left $ ParserFail err
         Right card -> return $ Right card

-- Fetches the ContactCard at the given TxHash
fetchContactCard :: TxHash -> IO (Either Web3Error ContactCard)
fetchContactCard hash = do
  eith <- runUser $ getTransactionByHash hash
  case eith of
    Left err -> return $ Left err
    Right maybeTx -> do
              case maybeTx of
                Nothing -> return $ Left $ ParserFail "Bad transaction hash"
                Just tx -> parseContactCard $ txInput tx

-- Imports contact at hash into address book with given email
importContact :: String -> TxHash -> IO ()
importContact email hash = do
  card <- fetchContactCard hash
  contact <- lookupContact email
  file <- contactFile
  case contact of
    Just _ -> error $ "Contact already exists with email address: " ++ email
    Nothing ->
        case card of
          Left err -> error $ show err
          Right crd -> addContact file $ Contact email hash crd
