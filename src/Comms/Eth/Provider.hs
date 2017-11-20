module Comms.Eth.Provider where

import           Control.Monad.IO.Class

import           Network.Ethereum.Web3
import           Network.Ethereum.Web3.Types

import           Comms.Common.Types
import           Comms.Common.Util

data UserProvider

-- Uses user-provided Ethereum node address
instance Provider UserProvider where
  rpcUri =
    Web3 $ do
      cfg <- getDefaultConfig
      return $
        case serverURL cfg of
          Just u  -> u
          Nothing -> "http://localhost:8545"

-- Web3 runner for UserProvider
runUser :: MonadIO m => Web3 UserProvider b -> m (Either Web3Error b)
{-# INLINE runUser #-}
runUser = runWeb3'
