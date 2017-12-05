{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Comms.Eth.Scanner where

import           Comms.Types
import           Comms.Common.Util
import           Comms.Eth.Cost
import           Comms.Eth.Provider

import           Control.Monad
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
                    case blockNum of
                      Left err -> return $ Left err
                      Right num -> return $ Right $ Pop3State num [] [] []
  else do
    bytes <- B.readFile path
    let d =  eitherDecode bytes :: Either String Pop3State
    case d of
      Left err  -> return $ Left $ ParserFail $ "Couldn't read pop3 state: " ++ err
      Right state -> return $ Right state
              
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

-- Writes pop3 state back to disk
writePop3State :: FilePath -> Pop3State -> IO ()
writePop3State path state = B.writeFile path $ encode state

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
  oState <- getPop3State stateFile
  events <- getMessages
  case events of
    Left err -> return $ Left err
    Right messages ->
          case oState of
            Left err -> return $ Left err
            Right oldState -> do
                        let newState = updatePop3State oldState messages
                            msgs = dropDeleted newState messages
                        size <- foldr (\el acc -> do
                                         a <- acc
                                         size <- messageSize $ changeData el
                                         return $ a + size
                                      ) (return 0) msgs
                        writePop3State stateFile newState
                        return $ Right $ "+OK " ++ (show $ length msgs) ++ " " ++ show size ++ "\r\n"
                       
dropDeleted :: Pop3State -> [a] -> [a]
dropDeleted state lst = fmap (\n -> lst!!n) ind
    where
      map = messageMap state
      del = pendingDeletion state
      ind = foldr (\e acc -> if not $ del!!e then e:acc else acc) [] map

-- POP3 LIST command
popList :: Maybe Int -> IO (Either Web3Error [String])
popList maybeNum = do
  st <- getPop3State stateFile
  events <- getMessages
  case events of
    Left err -> return $ Left err
    Right messages ->
        case st of
          Left err -> return $ Left err
          Right oldState -> do
                     let newState = updatePop3State oldState messages
                         map = messageMap newState
                     writePop3State stateFile newState
                     case maybeNum of
                       Just num -> do
                              if messageExists num newState then do
                                                              scan <- getScanListing num $ changeData $ messages!!(map!!(num - 1))
                                                              return $ Right $ ["+OK " ++ scan ++ ".\r\n"]
                              else return $ Right $ ["-ERR message " ++ show num ++ " already deleted\r\n"]
                       Nothing -> do
                              stat <- popStat
                              case stat of
                                Left err -> return $ Left err
                                Right hd -> do
                                          let msgs = dropDeleted newState messages
                                          resp <- ifoldr (\idx str resp -> do
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
popRetr :: Int -> IO (Either Web3Error String)
popRetr num = do
  st <- getPop3State stateFile
  events <- getMessages
  case events of
    Left err -> return $ Left err
    Right messages ->
        case st of
          Left err -> return $ Left err
          Right oldState -> do
                  let newState = updatePop3State oldState messages
                      map = messageMap newState
                  writePop3State stateFile newState
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
popDele :: Int -> IO (Either Web3Error String)
popDele num = do
  st <- getPop3State stateFile
  events <- getMessages
  case events of
    Left err -> return $ Left err
    Right messages ->
        case st of
          Left err -> return $ Left err
          Right oldState -> do
                  let newState = updatePop3State oldState messages
                      map = messageMap newState
                  writePop3State stateFile newState
                  if messageExists num newState then do
                     let pending = pendingDeletion newState
                         newNewState = newState { pendingDeletion = replaceAtIndex (map!!(num - 1)) True pending }
                     writePop3State stateFile newNewState
                     return $ Right $ "+OK message " ++ show num ++ " deleted\r\n"
                  else return $ Right $ "-ERR message " ++ show num ++ " already deleted\r\n"
                       
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
          
