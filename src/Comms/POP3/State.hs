{-# LANGUAGE OverloadedStrings #-}

module Comms.POP3.State where

import           Comms.Types
import           Control.Concurrent.STM
import qualified Data.ByteString.Lazy          as B
import           Data.Monoid            ((<>))
import qualified Data.Text              as T

import           Comms.Eth.Provider
import           Data.Aeson
import           Network.Ethereum.Web3.Eth     as E
import           Network.Ethereum.Web3.Types
import           System.Directory

newEmptyPOP3MVar = atomically newEmptyTMVar

overwritePOP3Session :: TMVar a -> a -> IO ()
overwritePOP3Session mvar env = do
  atomically $ tryTakeTMVar mvar
  atomically $ putTMVar mvar env

updateUser :: TMVar POP3Session -> T.Text -> IO ()
updateUser mvar newUser = do
  putStrLn "Entered updateUser"
  old <- atomically $ tryTakeTMVar mvar
  putStrLn "updateUser - Took TMVar"
  case old of
    Nothing -> do
      putStrLn "updateUser - Nothing Case."
      overwritePOP3Session mvar (POP3Session UNAUTH newUser "")
    Just oldSess -> do
      putStrLn "updateUser - Just Case."
      overwritePOP3Session mvar (oldSess {user = newUser})

updatePass :: TMVar POP3Session -> T.Text -> IO ()
updatePass mvar newPass = do
  putStrLn "Entered updatePass"
  old <- atomically $ tryTakeTMVar mvar
  putStrLn "updatePass - Took TMVar"
  case old of
    Nothing -> do
      putStrLn "updatePass - Nothing Case."
      overwritePOP3Session mvar (POP3Session TRANS "" newPass)
    Just oldSess -> do
      putStrLn "updatePass - Just Case."
      overwritePOP3Session mvar (oldSess {pass = newPass})

newEmptyInboxState = atomically $ newTMVar $ do
                       eitherState <- getPop3State stateFile
                       case eitherState of
                         Left err -> error $ show err
                         Right state -> return state

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
      Right state -> return $ Right $ state { pendingDeletion = [] }

-- Writes pop3 state back to disk
writePop3State :: FilePath -> Pop3State -> IO ()
writePop3State path state = B.writeFile path $ encode state
