{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Comms.Eth.Scanner where

import           Comms.Common.Types
import           Comms.Common.Util
import           Comms.Eth.Cost
import           Comms.Eth.Provider

import           Data.Aeson
import           Data.Maybe
import qualified Data.ByteArray                as BA
import qualified Data.ByteString.Char8 as C    
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

stateFile :: FilePath
stateFile = "state.json"

-- Gets pop3 state from disk or generates a new one
getPop3State :: FilePath -> IO (Either Web3Error Pop3State)
getPop3State path = do
  exists <- doesFileExist path
  if (not exists) then do
                    blockNum <- runUser $ E.blockNumber
                    return $ case blockNum of
                               Left err -> Left err
                               Right num -> Right $ Pop3State num [] []
  else do
    bytes <- B.readFile path
    let d =  eitherDecode bytes :: Either String Pop3State
    return $ case d of
               Left err  -> Left $ ParserFail $ "Couldn't read pop3 state: " ++ err
               Right state -> Right state

-- Writes pop3 state back to disk
writePop3State :: FilePath -> Pop3State -> IO ()
writePop3State path state = B.writeFile path $ encode state

-- Generates the new deletedMessages list
newInboxState :: [a] -> [Bool] -> [Bool]
newInboxState lst status = status ++ (replicate (length lst - length status) False)
                           
-- https://stackoverflow.com/a/10133429
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

-- Commits pendingDeletions to deletedMessages
commitDeletions :: Pop3State -> Pop3State
commitDeletions oldState = oldState { deletedMessages = newStatus, pendingDeletion = replicate (length newStatus) False }
  where
    oldStatus = deletedMessages oldState
    pending = pendingDeletion oldState
    newStatus = zipWith (||) oldStatus pending

-- Gets list of inbox messages from transaction log
getMessages :: IO (Either Web3Error [Change])
getMessages = do
  maybeState <- getPop3State stateFile
  case maybeState of
    Left err -> return $ Left err
    Right state -> do
                    contract <- getContract contractFile
                    config <- getDefaultConfig
                    let topic0 = eventHash "Message" contract
                        topic1 = T.append "0x" . T.justifyRight 64 '0' . toText . walletAddr $ config
                    runUser $ getLogs $ Filter Nothing (Just $ [Just topic0, Just topic1]) (Just $ startBlock state) Nothing

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
popStat :: IO (Either Web3Error String)
popStat = do
  events <- getMessages
  oldState <- getPop3State stateFile
  case events of
    Left err -> return $ Left err
    Right messages ->
          case oldState of
            Left err -> return $ Left err
            Right old -> do
                          let newDeleted = newInboxState messages (deletedMessages old)
                              newPending = newInboxState messages (pendingDeletion old)
                              newState = old { deletedMessages = newDeleted, pendingDeletion = newPending }
                              inbox = dropDeleted messages (zipWith (||) newDeleted newPending)
                          size <- foldr (\el acc -> do
                                           a <- acc
                                           size <- messageSize $ changeData el
                                           return $ a + size
                                        ) (return 0) inbox
                          writePop3State stateFile newState
                          return $ Right $ "+OK " ++ (show $ length inbox) ++ " " ++ show size ++ "\r\n"
                       
dropDeleted :: [a] -> [Bool] -> [a]
dropDeleted lst [] = lst
dropDeleted [] deleted = error "Invalid state -- should never occur"
dropDeleted (l:lst) (d:deleted) = if d then dropDeleted lst deleted
                                  else l:(dropDeleted lst deleted)

-- POP3 LIST command
popList :: Maybe Int -> IO (Either Web3Error [String])
popList maybeNum = do
  events <- getMessages
  case events of
    Left err -> return $ Left err
    Right messages ->
        case maybeNum of
          Just num -> do
                     valid <- messageExists num
                     case valid of
                       Left err -> return $ Left err
                       Right maybeError ->
                           case maybeError of
                             Just err -> return $ Right [err]
                             Nothing -> do
                                  scan <- getScanListing num $ changeData $ messages!!(num - 1)
                                  return $ Right $ ["+OK " ++ scan]
          Nothing -> do
                     stat <- popStat
                     case stat of
                       Left err -> return $ Left err
                       Right hd -> do
                                 resp <- ifoldr (\idx str resp -> do
                                                   s <- getScanListing (idx + 1) $ changeData $ str
                                                   r <- resp
                                                   return $ s:r
                                                ) (return [".\r\n"]) messages
                                 return $ Right $ hd:resp

getScanListing :: Int -> Text -> IO String
getScanListing num txData = do
  size <- messageSize txData
  return $ show num ++ " " ++ show size ++ "\r\n"
                
-- POP3 RETR command
popRetr :: Int -> IO (Either Web3Error String)
popRetr num = do
  events <- getMessages
  valid <- messageExists num
  case events of
    Left err -> return $ Left err
    Right messages ->
        case valid of
          Left err -> return $ Left err
          Right maybeError ->
              case maybeError of
                Just err -> return $ Right err
                Nothing -> do
                           let txData = changeData $ messages!!(num - 1)
                           msg <- decryptMessage $ parseMessage txData
                           size <- messageSize txData
                           let line1 = "+OK " ++ show size ++ " octets\r\n"
                           return $ Right $ line1 ++ msg ++ "\r\n.\r\n"

-- Returns Nothing on success, error message on failure
messageExists :: Int -> IO (Either Web3Error (Maybe String))
messageExists num = do
  st <- getPop3State stateFile
  case st of
    Left err -> return $ Left err
    Right state ->
        if num < 1 || num > len then return $ Right $ Just "-ERR invalid message number\r\n"
        else
            if pending!!(num - 1) || deleted!!(num - 1) then return $ Right $ Just $ "-ERR message " ++ show num ++ " already deleted\r\n"
            else return $ Right Nothing
        where
          pending = pendingDeletion state
          deleted = deletedMessages state
          len = length deleted

-- POP3 DELE command
popDele :: Int -> IO (Either Web3Error String)
popDele num = do
  oldState <- getPop3State stateFile
  valid <- messageExists num
  case oldState of
    Left err -> return $ Left err
    Right old ->
        case valid of
          Left err -> return $ Left err
          Right maybeError ->
              case maybeError of
                Just err -> return $ Right err
                Nothing -> do
                     let newState = old { pendingDeletion = replaceAtIndex (num - 1) True pending }
                     writePop3State stateFile newState
                     return $ Right $ "+OK message " ++ show num ++ " deleted\r\n"
        where
          pending = pendingDeletion old
          deleted = deletedMessages old
          len = length deleted

-- POP3 NOOP command
popNoop :: IO (Either Web3Error String)
popNoop = return $ Right "+OK\r\n"

-- POP3 RSET command
popRset :: IO (Either Web3Error String)
popRset = do
  oldState <- getPop3State stateFile
  case oldState of
    Left err -> return $ Left err
    Right old -> do
                  let pending = pendingDeletion old
                      len = length pending
                      newState = old { pendingDeletion = replicate len False }
                  writePop3State stateFile newState
                  popStat

-- POP3 QUIT command
popQuit :: IO (Either Web3Error String)
popQuit = do
  oldState <- getPop3State stateFile
  case oldState of
    Left err -> return $ Left err
    Right old -> do
                  let newState = commitDeletions old
                  writePop3State stateFile newState
                  return $ Right "+OK\r\n"
          
