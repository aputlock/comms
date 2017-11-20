{-# LANGUAGE DeriveGeneric #-}

module Comms.Common.Util where

import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import           GHC.Generics
import           Network.Ethereum.Web3.Address

data Config = Config
  { walletId :: !Address
  } deriving (Show, Generic)

instance FromJSON Config

instance ToJSON Config

getDefaultConfig :: IO Config
getDefaultConfig = getConfig "config.json"

getConfig :: FilePath -> IO Config
getConfig path = do
  d <- (eitherDecode <$> (B.readFile path)) :: IO (Either String Config)
  case d of
    Left err  -> error $ "bad config: " ++ err
    Right cfg -> return cfg

fromRight :: Either String b -> b
fromRight e =
  case e of
    Left err  -> error err
    Right val -> val
