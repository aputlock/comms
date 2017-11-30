{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}

module Comms.Common.Types where

import           Control.Concurrent.STM
import           Control.Monad.State
import           Data.Aeson
import qualified Data.ByteString               as S
import qualified Data.ByteString.Lazy          as B
import           Data.Data
import qualified Data.Text                     as T
import           Data.Typeable
import           GHC.Generics
import           Network.Ethereum.Web3.Address

-- | Defines the data type for commandline arguments.
data Options
  = RunServerOptions { config :: FilePath -- Defines the location of the configuration file.
                     , debug  :: Bool -- Enables debug printing.
                      }
  | ImportContact { }
  deriving (Data, Typeable, Show, Eq)

-- | Defines the schema for the config file.
data Config = Config
  { walletId :: !Address
  , smtpPort :: Int
  , pop3Port :: Int
  } deriving (Show, Generic)

instance FromJSON Config

instance ToJSON Config

{-| The states that a session can be in.-}
data SMTPState
  = Unknown
  | HaveHelo
  | HaveEhlo
  | HaveMailFrom
  | HaveRcptTo
  | HaveData
  | HaveQuit
  deriving (Show, Eq)

data Envelope = Envelope
  { client   :: T.Text
  , from     :: T.Text
  , to       :: [T.Text]
  , state    :: SMTPState
  , contents :: T.Text
  } deriving (Show, Eq)

data SMTPReply = Reply
  { statusCode :: Int
  , replyText  :: T.Text
  } deriving (Eq)

instance Show SMTPReply where
  show (Reply n str) =
    case n of
      0 -> T.unpack $ (T.append str "\r\n")
      _ -> show n ++ T.unpack (T.append (T.cons ' ' str) "\r\n")

type EnvelopeMVar = TMVar Envelope
