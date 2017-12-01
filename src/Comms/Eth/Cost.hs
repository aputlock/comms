{-# LANGUAGE OverloadedStrings #-}

module Comms.Eth.Cost where

import           Control.Monad.IO.Class

import           Network.Ethereum.Web3
import           Network.Ethereum.Web3.Address
import           Network.Ethereum.Web3.Eth
import           Network.Ethereum.Web3.JsonAbi
import           Network.Ethereum.Web3.TH
import           Network.Ethereum.Web3.Types

import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import           Data.List
import           Data.Maybe
import qualified Data.Text                     as T
import           Numeric

import           Comms.Types
import           Comms.Common.Util
import           Comms.Eth.Provider

-- Current address of deployed smart contract
contractAddr :: Address
contractAddr = "0x93998F7daF04B6c093255da91f9486c26c2e442c"

-- Returns registered account balance in Wei
getMyBalance :: MonadIO m => m Wei
getMyBalance = do
  cfg <- liftIO $ getDefaultConfig
  bal <- runUser $ getBalance (walletAddr cfg) Latest
  return $ textToWei bal

-- Returns estimated cost of function call in Wei
estimateCost :: MonadIO m => Call -> m Wei
estimateCost cl = do
  txtGas <- runUser $ estimateGas cl
  txtPrice <- runUser $ gasPrice
  return $ (textToWei txtGas * textToWei txtPrice)

-- Unsafely parses hex text into Wei value
textToWei :: Either Web3Error Text -> Wei
textToWei txt =
  case txt of
    Left err -> error $ "bad balance: " ++ show err
    Right t  -> fst $ head $ readHex $ drop 2 $ T.unpack t

-- Returns parsed contract abi at path
getContract :: FilePath -> IO ContractABI
getContract path = do
  d <- (eitherDecode <$> (B.readFile path)) :: IO (Either String ContractABI)
  case d of
    Left err  -> error $ "bad contract: " ++ err
    Right cfg -> return cfg

methodHash :: String -> ContractABI -> Text
methodHash methodName contract = methodId $ findDecl methodName $ unABI contract

eventHash :: String -> ContractABI -> Text
eventHash eventName contract = eventId $ findDecl eventName $ unABI contract
                                 
-- Builds a Call object for the given contract/method
getCall :: ContractABI -> String -> IO Call
getCall contract methodName = do
  cfg <- getDefaultConfig
  return $
    Call
    { callFrom = Just $ walletAddr cfg
    , callTo = contractAddr
    , callGas = Nothing
    , callGasPrice = Nothing
    , callValue = Nothing
    , callData =
        Just $
        T.justifyLeft 74 '0' $ methodHash methodName contract
    --  ^ concat(sha3(methodname), justifyRight(args)) **currently ignoring args
    }

-- Returns the Declaration with the given name
findDecl :: String -> [Declaration] -> Declaration
findDecl name lst = fromJust $ find (\e -> case e of
                                             DFunction fun _ _ _ -> fun == T.pack name
                                             DEvent e _ _ ->  e == T.pack name
                                             _ -> False) lst
