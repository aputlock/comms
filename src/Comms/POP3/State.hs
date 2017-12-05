{-# LANGUAGE OverloadedStrings #-}

module Comms.POP3.State where

import           Comms.Types
import           Control.Concurrent.STM
import           Data.Monoid            ((<>))
import qualified Data.Text              as T

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

newEmptyInboxState = atomically newEmptyTMVar      
