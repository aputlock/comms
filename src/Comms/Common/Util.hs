module Comms.Common.Util where

import           Comms.Common.Types
import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import           GHC.Generics
import           Network.Ethereum.Web3.Address

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

fromRight :: Either String b -> b
fromRight e =
  case e of
    Left err  -> error err
    Right val -> val
