{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Comms.Eth.Scanner where

import           Comms.Types
import           Comms.Common.Util
import           Comms.Eth.Cost
import           Comms.Eth.Provider

import           Control.Monad
import           Control.Monad.STM
import           Control.Concurrent.STM.TMVar
import           Data.Aeson
import           Data.Maybe
import qualified Data.ByteArray                as BA
import qualified Data.ByteString.Char8         as C    
import qualified Data.ByteString.Lazy          as B
import           Data.List.Index
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Read                as TR
import           GHC.Generics
import           System.Directory
    
import           Network.Ethereum.Web3
import           Network.Ethereum.Web3.Address
import           Network.Ethereum.Web3.Contract
import           Network.Ethereum.Web3.Eth     as E
import           Network.Ethereum.Web3.TH
import           Network.Ethereum.Web3.Types
              
-- Updates pop3 state with new messages
updatePop3State :: Pop3State -> [Change] -> Pop3State
updatePop3State oldState messages = 
    oldState { deletedMessages = del
             , pendingDeletion = pnd
             , messageMap = generateMessageMap messages del
             }
    where
      del = extendState messages $ deletedMessages oldState
      pnd = extendState messages $ pendingDeletion oldState

-- Generates the new deletedMessages list
extendState :: [a] -> [Bool] -> [Bool]
extendState lst status = status ++ (replicate (length lst - length status) False)
                           
-- https://stackoverflow.com/a/10133429
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

-- Commits pendingDeletions to deletedMessages
commitDeletions :: Pop3State -> Pop3State
commitDeletions oldState = oldState { deletedMessages = newStatus
                                    , pendingDeletion = replicate (length newStatus) False
                                    , messageMap = generateMessageMap newStatus newStatus -- TODO: is this right?
                                    }
  where
    oldStatus = deletedMessages oldState
    pending = pendingDeletion oldState
    newStatus = zipWith (||) oldStatus pending

-- Gets a mapping of pop3 inbox index to message list index
generateMessageMap :: [a] -> [Bool] -> [Int]
generateMessageMap lst deleted = mappingHelper 0 lst
    where
      mappingHelper _ [] = []
      mappingHelper i (hd:tl) =
          if not $ deleted!!i then
              i:(mappingHelper (i + 1) tl)
          else
              mappingHelper (i + 1) tl

-- Gets list of inbox messages from transaction log
getMessages :: InboxState -> IO (Either Web3Error [Change])
getMessages state = do
  st <- atomically $ readTMVar state
  do
    curState <- st
    config <- getDefaultConfig
    let contract = getContract contractFile
        topic0 = eventHash "Message" contract
        topic1 = T.append "0x" . T.justifyRight 64 '0' . toText . walletAddr $ config
    runUser $ getLogs $ Filter Nothing (Just $ [Just topic0, Just topic1]) (Just $ startBlock curState) Nothing

-- Parse utf16-encoded utf8 string
parseUtf16 :: String -> String
parseUtf16 = T.unpack . T.decodeUtf8 . C.pack

-- Parse encrypted message text from transaction data
parseMessage :: Text -> String
parseMessage txt = parseUtf16 . T.unpack . hexToAscii . T.take len $ T.drop 128 args
    where
      args = T.drop 2 txt
      offset = 2 * (fst $ fromRight $ TR.hexadecimal (T.take 64 args))
      len = 2 * (fst $ fromRight $ TR.hexadecimal $ T.take 64 (T.drop offset args))

-- Returns size in bytes of the message in the given transaction data
messageSize :: Text -> IO Int
messageSize txData = do
  msg <- decryptMessage $ parseMessage txData
  return $ length msg

-- POP3 STAT command
popStat :: InboxState -> IO (Either Web3Error String)
popStat state = do
  st <- atomically $ readTMVar state
  do
    oldState <- st
    events <- getMessages state
    case events of
      Left err -> return $ Left err
      Right messages -> do
                  let newState = updatePop3State oldState messages
                      msgs = dropDeleted newState messages
                  size <- foldr (\(_, el) acc -> do
                                   a <- acc
                                   size <- messageSize $ changeData el
                                   return $ a + size
                                ) (return 0) msgs
                  atomically $ swapTMVar state $ return newState
                  return $ Right $ "+OK " ++ (show $ length msgs) ++ " " ++ show size ++ "\r\n"
                       
dropDeleted :: Pop3State -> [a] -> [(Int, a)]
dropDeleted state lst = fmap (\(idx, n) -> (idx, lst!!n)) ind
    where
      map = messageMap state
      del = pendingDeletion state
      ind = ifoldr (\idx e acc -> if not $ del!!e then (idx, e):acc else acc) [] map


-- POP3 UIDL command
popUidl :: InboxState -> Maybe Int -> IO (Either Web3Error [String])
popUidl state maybeNum = do
  st <- atomically $ readTMVar state
  do
    oldState <- st
    events <- getMessages state
    case events of
      Left err -> return $ Left err
      Right messages -> do
                  let newState = updatePop3State oldState messages
                      map = messageMap newState
                  atomically $ swapTMVar state $ return newState
                  case maybeNum of
                    Just num -> do
                           if messageExists num newState then do
                                                           let scan = changeTransactionHash $ messages!!(map!!(num - 1))
                                                           return $ Right $ ["+OK " ++ T.unpack scan ++ ".\r\n"]
                           else return $ Right $ ["-ERR message " ++ show num ++ " already deleted\r\n"]
                    Nothing -> do
                           stat <- popStat state
                           case stat of
                             Left err -> return $ Left err
                             Right hd -> do
                                       let msgs = dropDeleted newState messages
                                       resp <- foldr (\(idx, str) resp -> do
                                                         s <- return $ show (idx + 1) ++ " " ++ (T.unpack $ changeTransactionHash $ str) ++ "\r\n"
                                                         r <- resp
                                                         return $ s:r
                                                      ) (return [".\r\n"]) msgs
                                       return $ Right $ hd:resp

-- POP3 LIST command
popList :: InboxState -> Maybe Int -> IO (Either Web3Error [String])
popList state maybeNum = do
  st <- atomically $ readTMVar state
  do
    oldState <- st
    events <- getMessages state
    case events of
      Left err -> return $ Left err
      Right messages -> do
                  let newState = updatePop3State oldState messages
                      map = messageMap newState
                  atomically $ swapTMVar state $ return newState
                  case maybeNum of
                    Just num -> do
                           if messageExists num newState then do
                                                           scan <- getScanListing num $ changeData $ messages!!(map!!(num - 1))
                                                           return $ Right $ ["+OK " ++ scan ++ ".\r\n"]
                           else return $ Right $ ["-ERR message " ++ show num ++ " already deleted\r\n"]
                    Nothing -> do
                           stat <- popStat state
                           case stat of
                             Left err -> return $ Left err
                             Right hd -> do
                                       let msgs = dropDeleted newState messages
                                       resp <- foldr (\(idx, str) resp -> do
                                                         s <- getScanListing (idx + 1) $ changeData $ str
                                                         r <- resp
                                                         return $ s:r
                                                      ) (return [".\r\n"]) msgs
                                       return $ Right $ hd:resp

getScanListing :: Int -> Text -> IO String
getScanListing num txData = do
  size <- messageSize txData
  return $ show num ++ " " ++ show size ++ "\r\n"
                
-- POP3 RETR command
popRetr :: InboxState -> Int -> IO (Either Web3Error String)
popRetr state num = do
  st <- atomically $ readTMVar state
  do
    oldState <- st
    events <- getMessages state
    case events of
      Left err -> return $ Left err
      Right messages -> do
                      let newState = updatePop3State oldState messages
                          map = messageMap newState
                      atomically $ swapTMVar state $ return newState
                      if messageExists num newState then do
                                                      let txData = changeData $ messages!!(map!!(num - 1))
                                                      msg <- decryptMessage $ parseMessage txData
                                                      size <- messageSize txData
                                                      let line1 = "+OK " ++ show size ++ " octets\r\n"
                                                      return $ Right $ line1 ++ msg ++ "\r\n.\r\n"
                      else return $ Right $ "-ERR messsage " ++ show num ++ " already deleted\r\n"

-- Returns Nothing on success, error message on failure
messageExists :: Int -> Pop3State -> Bool
messageExists num state = (n >= 0) && (n < len) && (not $ del!!(map!!n))
    where
      n = num - 1
      map = messageMap state
      pending = pendingDeletion state
      deleted = deletedMessages state
      del = zipWith (||) pending deleted
      len = length map

-- POP3 DELE command
popDele :: InboxState -> Int -> IO (Either Web3Error String)
popDele state num = do
  st <- atomically $ readTMVar state
  do
    oldState <- st
    events <- getMessages state
    case events of
      Left err -> return $ Left err
      Right messages -> do
                      let newState = updatePop3State oldState messages
                          map = messageMap newState
                      atomically $ swapTMVar state $ return newState
                      if messageExists num newState then do
                                                      let pending = pendingDeletion newState
                                                          newNewState = newState { pendingDeletion = replaceAtIndex (map!!(num - 1)) True pending }
                                                      atomically $ swapTMVar state $ return newNewState
                                                      return $ Right $ "+OK message " ++ show num ++ " deleted\r\n"
                      else return $ Right $ "-ERR message " ++ show num ++ " already deleted\r\n"
                       
-- POP3 NOOP command
popNoop :: InboxState -> IO (Either Web3Error String)
popNoop _ = return $ Right "+OK\r\n"

-- POP3 RSET command
popRset :: InboxState -> IO (Either Web3Error String)
popRset state = do
  st <- atomically $ readTMVar state
  do
    oldState <- st
    let pending = pendingDeletion oldState
        len = length pending
        newState = oldState { pendingDeletion = replicate len False }
    atomically $ swapTMVar state $ return newState
    popStat state

-- POP3 QUIT command
popQuit :: InboxState -> IO (Either Web3Error String)
popQuit state = do
  st <- atomically $ readTMVar state
  do
    oldState <- st
    let newState = commitDeletions oldState
    atomically $ swapTMVar state $ return newState
    return $ Right "+OK\r\n"
