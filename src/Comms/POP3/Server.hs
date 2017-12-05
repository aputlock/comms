{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Comms.POP3.Server
  ( handleConn
  ) where

import           Comms.Common.Util
import           Comms.Eth.Scanner
import qualified Comms.POP3.Handler     as H
import           Comms.POP3.State
import           Comms.Types

import           Control.Concurrent.STM
import           Control.Monad          (forM_)
import qualified Data.Text              as T
import qualified Data.Text.Read         as TR
import           Network
import           System.IO

handleConn :: Handle -> Config -> t -> IO ()
handleConn handle config channel = do
  inboxState <- newEmptyInboxState
  startSession handle inboxState
  do
    st <- atomically $ takeTMVar inboxState
    do
      state <- st
      let storedState = state { pendingDeletion = [] }
      writePop3State stateFile storedState
      putStrLn "-=-=-=-Closing POP3 Session-=-=-=-"

startSession :: Handle -> InboxState -> IO ()
startSession handle inboxState = do
  putStrLn "-=-=-=-Starting POP3 Session-=-=-=-"
  env <- newEmptyPOP3MVar
  H.sendReply handle $
    POP3Reply OK "pop3.localhost.mail Comms POP3 Server Ready"
  sessLoop handle env inboxState

sessLoop :: Handle -> POP3MVar -> InboxState -> IO ()
sessLoop handle env inboxState = do
  putStrLn "Top of POP3 sessLoop"
  line <- hGetLine handle
  let t = T.pack line
  case verb t of
    "CAPA" -> do
      H.capa handle t env inboxState
      sessLoop handle env inboxState
    "USER" -> do
      H.user handle t env inboxState
      sessLoop handle env inboxState
    "PASS" -> do
      H.pass handle t env inboxState
      sessLoop handle env inboxState
    "STAT" -> do
      H.stat handle t env inboxState
      sessLoop handle env inboxState
    "LIST" -> do
      H.list handle t env inboxState
      sessLoop handle env inboxState
    "UIDL" -> do
      H.uidl handle t env inboxState
      sessLoop handle env inboxState
    "RETR" -> do
      H.retr handle t env inboxState
      sessLoop handle env inboxState
    "DELE" -> do
      H.dele handle t env inboxState
      sessLoop handle env inboxState
    "QUIT" -> do
      H.quit handle t env inboxState
      return ()
    "NOOP" -> do
      H.noop handle t env inboxState
      sessLoop handle env inboxState
    "RSET" -> do
      H.rset handle t env inboxState
      sessLoop handle env inboxState
    _ -> do
      H.sendReply handle (POP3Reply ERR "command not supported")
      sessLoop handle env inboxState
