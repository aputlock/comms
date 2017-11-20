module Comms.Eth.Tools where

import Control.Monad.IO.Class
import Network.Ethereum.Web3
import Network.Ethereum.Web3.Eth
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.TH

getFirstBalance :: MonadIO m => m (Either Web3Error Text)
getFirstBalance = do
  acc <- runWeb3 accounts
  bal <- runWeb3 $ getBalance (head $ extractList acc) Latest
  return bal

extractList :: (Either Web3Error [a]) -> [a]
extractList (Right e) = e
extractList (Left _) = [] -- handle error instead
