{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Comms.Common.Types where

import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import           Data.Data
import           Data.Typeable
import           GHC.Generics
import           Network.Ethereum.Web3.Address

-- | Defines the data type for commandline arguments.
data Options = Options
  { config :: FilePath -- Defines the location of the configuration file.
  , debug  :: Bool -- Enables debug printing.
  } deriving (Data, Typeable, Show, Eq)

-- | Defines the schema for the config file.
data Config = Config
  { walletId :: !Address
  , smtpPort :: Int
  , imapPort :: Int
  } deriving (Show, Generic)

instance FromJSON Config

instance ToJSON Config
