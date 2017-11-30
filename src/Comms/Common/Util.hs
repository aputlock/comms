module Comms.Common.Util where

import           Comms.Common.Types
import           Control.Concurrent            (forkFinally)
import           Control.Exception
import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import           GHC.Generics
import           Network                       (Socket (..), accept)
import           Network.Ethereum.Web3.Address
import           System.IO                     (BufferMode (NoBuffering),
                                                Handle, hClose, hSetBuffering)

getDefaultConfig :: IO Config
getDefaultConfig = getConfig "config.json"

getConfig :: FilePath -> IO Config
getConfig path = do
  d <- (eitherDecode <$> B.readFile path) :: IO (Either String Config)
  case d of
    Left err  -> error $ "bad config: " ++ err
    Right cfg -> return cfg

getConfigFromOptions :: Options -> IO Config
getConfigFromOptions opt = getConfig (config opt)

getDebug :: Options -> Bool
getDebug opts = debug opts

getSMTPPort :: Config -> Int
getSMTPPort = smtpPort

getPOP3Port :: Config -> Int
getPOP3Port = pop3Port

fromRight :: Either String b -> b
fromRight e =
  case e of
    Left err  -> error err
    Right val -> val

-- Major event loop
bindServer :: Socket -> (Handle -> Config -> t -> IO ()) -> Config -> t -> IO ()
bindServer sock handler config channel = do
  bracket
    (accept sock)
    (\(handle, _, _) -> hClose handle)
    (\(handle, _, _) -> do
       putStrLn "In Handler"
       -- Need to process incoming connections w/o buffering
       hSetBuffering handle NoBuffering
       -- TODO(broluwo): Consider moving to slave threads in order to catch exceptions more cleanly.
       forkFinally (handler handle config channel) (closeHandle handle)
       putStrLn "Forked handler"
       bindServer sock handler config channel)

--FIXME(broluwo): Don't swallow the exception, move to using slave threads or the async lib.
closeHandle handle (Left e) = do
  hClose handle
  return ()
closeHandle handle (Right r) = do
  hClose handle
  putStrLn "Closed SMTP session handle"
  return r
