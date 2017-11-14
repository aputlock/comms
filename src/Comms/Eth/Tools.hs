module Comms.Eth.Tools where

import Control.Monad.IO.Class
import Network.Ethereum.Web3
import Network.Ethereum.Web3.Api
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.TH

getBalance :: MonadIO m => m (Either Web3Error Text)
getBalance = do
  acc <- runWeb3 eth_accounts
  bal <- runWeb3 $ eth_getBalance (head $ extractList acc) Latest
  return bal

extractList :: (Either Web3Error [a]) -> [a]
extractList (Right e) = e
extractList (Left _) = [] -- handle error instead
