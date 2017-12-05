{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Comms.Types where

import           Control.Concurrent.STM
import           Control.Monad.State
import           Crypto.Types.PubKey.RSA
import           Data.Aeson
import qualified Data.ByteString               as S
import qualified Data.ByteString.Lazy          as B
import           Data.Data
import qualified Data.Text                     as T
import           Data.Typeable
import           GHC.Generics
import           Network.Ethereum.Web3.Address
import           Network.Ethereum.Web3.Types
import           System.IO                     (Handle (..))

-- | Defines the data type for commandline arguments.
data Options = RunServerOptions
  { config :: FilePath -- ^ Defines the location of the configuration file.
  , debug  :: Bool -- ^ Enables debug printing.
  } deriving (Data, Typeable, Show, Eq)

-- | Defines the schema for the config file.
data Config = Config
  { walletAddr :: !Address -- ^ The user's Ethereum wallet address.
  , smtpPort   :: Int -- ^ The port the SMTP server will run on.
  , pop3Port   :: Int -- ^ The port the POP3 server will run on.
  , serverURL  :: Maybe String -- ^ The URL of the Ethereum node. If not provided, defaults to localhost.
  , keyFile    :: Maybe FilePath -- ^ The location of the RSA keypair file.
  } deriving (Show, Generic)

instance FromJSON Config

instance ToJSON Config

data Contact = Contact
  { emailAddr   :: String
  , txHash      :: TxHash
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
    object
      [ "public_size" .= public_size
      , "public_n" .= public_n
      , "public_e" .= public_e
      ]

instance FromJSON PublicKey where
  parseJSON (Object v) =
    PublicKey <$> v .: "public_size" <*> v .: "public_n" <*> v .: "public_e"

instance ToJSON PrivateKey where
  toJSON (PrivateKey private_pub private_d private_p private_q private_dP private_dQ private_qinv) =
    object
      [ "private_pub" .= private_pub
      , "private_d" .= private_d
      , "private_p" .= private_p
      , "private_q" .= private_q
      , "private_dP" .= private_dP
      , "private_dQ" .= private_dQ
      , "private_qinv" .= private_qinv
      ]

instance FromJSON PrivateKey where
  parseJSON (Object v) =
    PrivateKey <$> v .: "private_pub" <*> v .: "private_d" <*> v .: "private_p" <*>
    v .: "private_q" <*>
    v .: "private_dP" <*>
    v .: "private_dQ" <*>
    v .: "private_qinv"

data Pop3State = Pop3State
  { startBlock      :: T.Text
  , deletedMessages :: [Bool]
  , pendingDeletion :: [Bool]
  } deriving (Show, Generic)

instance ToJSON Pop3State

instance FromJSON Pop3State

type InboxState = TMVar Pop3State

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
      0 -> T.unpack (T.append str "\r\n")
      _ -> show n ++ T.unpack (T.append (T.cons ' ' str) "\r\n")

type EnvelopeMVar = TMVar Envelope

type SMTPHandler = Handle -> T.Text -> EnvelopeMVar -> IO ()

data POP3SessionState
  = UNAUTH
  | TRANS
  | UPDATE
  deriving (Show, Eq)

data POP3Session = POP3Session
  { sessionState :: POP3SessionState
  , user         :: T.Text
  , pass         :: T.Text
  } deriving (Show, Eq)

data POP3Reply = POP3Reply
  { status :: POP3ReplyIndicator
  , text   :: T.Text
  }

instance Show POP3Reply where
  show (POP3Reply s t) = show s ++ T.unpack (T.append (T.cons ' ' t) "\r\n")

data POP3ReplyIndicator
  = NONE
  | OK
  | ERR
  deriving (Eq)

instance Show POP3ReplyIndicator where
  show NONE = ""
  show OK   = "+OK"
  show ERR  = "-ERR"

type POP3MVar = TMVar POP3Session
type POP3Handler = Handle -> T.Text -> POP3MVar -> InboxState -> IO ()
