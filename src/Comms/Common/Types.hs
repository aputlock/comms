{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

-- ^ For info about DeriveDataTypable, see http://chrisdone.com/posts/data-typeable
-- ^ OverloadedString allows String to be treated as either the type String or Data.Text
module Comms.Common.Types where

import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import           Data.Data
import           Data.Typeable
import           GHC.Generics
import           Network.Ethereum.Web3.Address

data Options = Options
       -- | Defines the location of the config file.
  { config :: FilePath
  -- | Enables debug printing.
  , debug  :: Bool
  } deriving (Data, Typeable, Show, Eq)

-- | Defines the schema for the config file.
data Config = Config
  { walletId  :: !Address
  , smtpPort  :: Int
  , imapPort  :: Int
  } deriving (Show, Generic)


instance FromJSON Config

instance ToJSON Config
