{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Comms.POP3.Server
  ( handleConn
  ) where

import           Comms.Common.Util
import           Comms.Types
import           Comms.Eth.Scanner
import Comms.POP3.State
import qualified Comms.POP3.Handler as H

import           Network
import           System.IO
import qualified Data.Text      as T
import qualified Data.Text.Read as TR
import           Control.Concurrent.STM
import           Control.Monad (forM_)

handleConn :: Handle -> Config -> t -> IO ()
handleConn handle config channel = do
  startSession handle
  putStrLn "-=-=-=-Closing POP3 Session-=-=-=-"

startSession :: Handle -> IO ()
startSession handle = do
  putStrLn "-=-=-=-Starting POP3 Session-=-=-=-"
  env <- newEmptyPOP3MVar
  H.sendReply handle $ POP3Reply OK "pop3.localhost.mail Comms POP3 Server Ready"
  sessLoop handle env

sessLoop :: Handle -> POP3MVar -> IO ()
sessLoop handle env = do
  putStrLn "Top of POP3 sessLoop"
  line <- hGetLine handle
  let t = T.pack line
  case verb t of
    "CAPA" -> do
      H.capa handle t env
      sessLoop handle env
    "USER" -> do
      H.user handle t env
      sessLoop handle env
    "PASS" -> do
      H.pass handle t env
      sessLoop handle env
    "STAT" -> do
      H.stat handle t env
      sessLoop handle env
    "LIST" -> do
      H.list handle t env
      sessLoop handle env
    "RETR" -> do
      H.retr handle t env
      sessLoop handle env
    "DELE" -> do
      H.dele handle t env
      sessLoop handle env
    "QUIT" -> do
      H.quit handle t env
      return ()
    "NOOP" -> do
      H.noop handle t env
      sessLoop handle env
    "RSET" -> do
      H.rset handle t env
      sessLoop handle env
    _ -> do
      H.sendReply handle (POP3Reply ERR "command not supported")
      sessLoop handle env
