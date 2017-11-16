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

import           Comms.Common.Types
import           Comms.Common.Util

-- Current address of deployed smart contract
contractAddr :: Address
contractAddr =
  fromRight $ fromText $ T.pack "0x4d9602D0c78096788d7B2A12cC6Dba891F1f21cA"

-- Returns registered account balance in Wei
getMyBalance :: MonadIO m => m Wei
getMyBalance = do
  cfg <- liftIO $ getDefaultConfig
  bal <- runWeb3 $ getBalance (walletId cfg) Latest
  return $ textToWei bal

-- Returns estimated cost of function call in Wei
estimateCost :: MonadIO m => IO Call -> m Wei
estimateCost c = do
  cl <- liftIO c
  txtGas <- runWeb3 $ estimateGas cl
  txtPrice <- runWeb3 $ gasPrice
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

-- Builds a Call object for the given contract/method
getCall :: IO ContractABI -> String -> IO Call
getCall ab methodName = do
  cfg <- getDefaultConfig
  contract <- ab
  return $
    Call
    { callFrom = Just $ walletId cfg
    , callTo = contractAddr
    , callGas = Nothing
    , callGasPrice = Nothing
    , callValue = Nothing
    , callData =
        Just $
        T.justifyLeft 74 '0' $ methodId $ findDecl methodName $ unABI contract
             --  ^ concat(sha3(methodname), justifyRight(args)) **currently ignoring args
    }

-- Returns the Declaration with the given name
findDecl :: String -> [Declaration] -> Declaration
findDecl name lst = fromJust $ find (\e -> funName e == T.pack name) lst
