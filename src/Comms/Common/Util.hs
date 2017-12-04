{-# LANGUAGE OverloadedStrings #-}

module Comms.Common.Util
  ( module Comms.Common.Util
  ) where

import qualified Codec.Crypto.RSA.Pure         as RSA
import           Control.Concurrent.Async
import           Control.Exception
import           Crypto.Random
import           Crypto.Types.PubKey.RSA
import           Data.Aeson
import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Object
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Read                as TR
import           GHC.Generics
import           Network                       (Socket (..), accept)
import           Network.Ethereum.Web3.Address
import           System.Directory
import           System.IO                     (BufferMode (NoBuffering),
                                                Handle, hClose, hSetBuffering)

import           Comms.Types

-- General Utilities
fromRight :: Show s => Either s b -> b
fromRight e =
  case e of
    Left err  -> error $ show err
    Right val -> val

-- Converts a hex string to ascii
hexToAscii :: T.Text -> T.Text
hexToAscii txt =
  if T.length txt `mod` 2 /= 0
    then error "Malformed hex string"
    else T.pack str
  where
    bytes = T.chunksOf 2 txt
    str = fmap (chr . fst . fromRight . TR.hexadecimal) bytes

-- Config Utilities
defaultConfig :: String
defaultConfig = "config.json"

defaultKeyFile :: String
defaultKeyFile = "rsa.key"

contractFile :: String
contractFile = "solidity/contract.json"

contactFile :: String
contactFile = "contacts.json"

getDefaultConfig :: IO Config
getDefaultConfig = getConfig defaultConfig

getConfig :: FilePath -> IO Config
getConfig path = do
  d <- (eitherDecode <$> B.readFile path) :: IO (Either String Config)
  case d of
    Left err  -> error $ "bad config: " ++ err
    Right cfg -> return cfg

getConfigFromOptions :: Options -> IO Config
getConfigFromOptions opt = getConfig (config opt)

writeConfig :: FilePath -> Config -> IO ()
writeConfig path cfg = B.writeFile path $ encode cfg

getContacts :: FilePath -> IO [Contact]
getContacts path = do
  exists <- doesFileExist path
  if not exists
    then return []
    else do
      bytes <- B.readFile path
      let d = eitherDecode bytes :: Either String [Contact]
      case d of
        Left err  -> error $ "bad contacts: " ++ err
        Right cfg -> return cfg

-- | Lookup contact from address book
lookupContact :: String -> IO (Maybe ContactCard)
lookupContact email = do
  contacts <- getContacts contactFile
  return $
    case find (\x -> emailAddr x == email) contacts of
      Nothing      -> Nothing
      Just contact -> Just $ contactInfo contact

addContact :: FilePath -> Contact -> IO ()
addContact path contact = do
  contacts <- getContacts path
  B.writeFile path $ encode $ contact : contacts

-- Crypto Utilities
writeKeyPair :: FilePath -> PrivateKey -> IO ()
writeKeyPair path key = B.writeFile path $ encodeASN1 DER $ toASN1 key []

getKeyPair :: FilePath -> IO PrivateKey
getKeyPair path = do
  bytes <- B.readFile path
  return $ fst $ fromRight $ fromASN1 $ fromRight $ decodeASN1 DER bytes

genKeyPair :: IO PrivateKey
genKeyPair = do
  gen <- newGenIO :: IO SystemRandom
  return $
    case RSA.generateKeyPair gen 4096 of
      Left err           -> error $ show err
      Right (_, priv, _) -> priv

-- | Hard-fail wrapper for RSA encrypt
encrypt :: String -> PublicKey -> IO String
encrypt plain key = do
  gen <- newGenIO :: IO SystemRandom
  return $ C.unpack $ fst $ fromRight $ RSA.encrypt gen key $ C.pack plain

-- | Hard-fail wrapper for RSA decrypt
decrypt :: String -> PrivateKey -> String
decrypt ciph key = C.unpack $ fromRight $ RSA.decrypt key $ C.pack ciph

-- | High-level decrypt function
decryptMessage :: String -> IO String
decryptMessage ciph = do
  cfg <- getDefaultConfig
  key <- getKeyPair $ fromMaybe "rsa.key" $ keyFile cfg
  return $ decrypt ciph key

getDebug :: Options -> Bool
getDebug = debug

getSMTPPort :: Config -> Int
getSMTPPort = smtpPort

getPOP3Port :: Config -> Int
getPOP3Port = pop3Port

{- | Server connection handler. An instance is spun up for both POP3 and SMTP.-}
bindServer :: Socket -> (Handle -> Config -> t -> IO ()) -> Config -> t -> IO ()
bindServer sock handler config channel = do
  putStrLn "Awaiting new connection..."
  withAsync (accept sock) $ \await -> do
    (handle, _, _) <- wait await
    hSetBuffering handle NoBuffering
    putStrLn "Received new connection. Spawning handler..."
    serverSession <- async (handler handle config channel)
    closingSession <- waitCatch serverSession
    closeHandle handle closingSession
    putStrLn "Session Closed."
    bindServer sock handler config channel

{- | Naive exception handler that simply prints the error if there is one. Regardless of the prescence of an exception the @Handle@ is closed.
-}
closeHandle :: Show a => Handle -> Either a () -> IO ()
closeHandle handle (Left e) = do
  print e
  hClose handle
  return ()
closeHandle handle (Right r) = do
  hClose handle
  putStrLn "Closed session handle"
  return r

{- | Get the string representation of the requested command -}
verb :: T.Text -> T.Text
verb str = T.strip $ head $ T.splitOn crlf $ head $ T.words str

{- | Get the argument side of a given SMTP/POP3 command -}
arg :: T.Text -> T.Text
arg str = T.stripEnd $ head $ T.splitOn crlf $ T.unwords $ tail $ T.words str

{- Wrap @arg@ with a Maybe, when arguments are optional. -}
maybeArg :: T.Text -> Maybe T.Text
maybeArg str =
  if length (T.words str) > 1
    then Just $ arg str
    else Nothing

{- | The ending character sequence for SMTP and POP3 commands -}
crlf :: T.Text
crlf = "\r\n"
