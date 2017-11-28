{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings #-}

module Comms.Common.Types where

import           Crypto.Types.PubKey.RSA
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
  { walletAddr :: !Address
  , smtpPort :: Int
  , imapPort :: Int
  , serverURL  :: Maybe String
  , keyFile    :: Maybe FilePath
  } deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config

data Contact = Contact
    { emailAddr :: String
    , contactInfo :: ContactCard
    } deriving (Show, Generic)

instance ToJSON Contact
instance FromJSON Contact

data ContactCard = ContactCard
    { inboxAddr :: !Address
    , publicKey :: PublicKey
    } deriving (Show, Generic)

instance ToJSON ContactCard
instance FromJSON ContactCard

instance ToJSON PublicKey where
    toJSON (PublicKey public_size public_n public_e) =
        object ["public_size" .= public_size, "public_n" .= public_n, "public_e" .= public_e]

instance FromJSON PublicKey where
    parseJSON (Object v) = PublicKey
                           <$> v .: "public_size"
                           <*> v .: "public_n"
                           <*> v .: "public_e"

instance ToJSON PrivateKey where
    toJSON (PrivateKey private_pub private_d private_p private_q private_dP private_dQ private_qinv) =
        object ["private_pub" .= private_pub, "private_d" .= private_d, "private_p" .= private_p, "private_q" .= private_q, "private_dP" .= private_dP, "private_dQ" .= private_dQ, "private_qinv" .= private_qinv]

instance FromJSON PrivateKey where
    parseJSON (Object v) = PrivateKey
                           <$> v .: "private_pub"
                           <*> v .: "private_d"
                           <*> v .: "private_p"
                           <*> v .: "private_q"
                           <*> v .: "private_dP"
                           <*> v .: "private_dQ"
                           <*> v .: "private_qinv"
