{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Comms.POP3
  ( handleConn
  ) where

import           Comms.Common.Util
import           Comms.Types
import           Comms.Eth.Scanner

import           Network
import           System.IO
import qualified Data.Text      as T
import qualified Data.Text.Read as TR
import           Control.Concurrent.STM
import           Control.Monad (forM_)

handleConn :: Handle -> Config -> t -> IO ()
handleConn handle config channel = do
  startSession handle
  putStrLn "Returning from thread"

startSession :: Handle -> IO ()
startSession handle = do
  putStrLn "In startSession"
  env <- newEmptyPOP3MVar
  sendReply handle $ POP3Reply OK "pop3.localhost.mail Comms POP3 Server Ready"
  sessLoop handle env

sessLoop :: Handle -> POP3MVar -> IO ()
sessLoop handle env = do
  putStrLn "Top of POP3 sessLoop"
  line <- hGetLine handle
  let t = T.pack line
  case verb t of
    "CAPA" -> do
      sendReply handle $ POP3Reply OK "Capability list follows"
      sendPreFormReply handle "USER\r\n"
      sendPreFormReply handle ".\r\n"
      sessLoop handle env
    "USER" -> do
      handleUser handle t env
      sessLoop handle env
    "PASS" -> do
      handlePass handle t env
      sessLoop handle env
    "STAT" -> do
      handleNoArg handle popStat
      sessLoop handle env
    "LIST" -> do
      case maybeArg t of
        Nothing -> do
          either <- popList Nothing
          case either of
            Left err -> sendReply handle (POP3Reply ERR $ T.pack $ show err)
            Right s ->  forM_ s (\res -> sendPreFormReply handle res)
        Just parsed  -> do
          let eitherNum = TR.decimal parsed
          case eitherNum of
            Left err -> sendReply handle (POP3Reply ERR $ T.pack $ show err)
            Right (num, _) -> do
              e <- popList $ Just num
              case e of
                Left err -> sendReply handle (POP3Reply ERR $ T.pack $ show err)
                Right s ->  forM_ s (\res -> sendPreFormReply handle res)
      sessLoop handle env
    "RETR" -> do
      handleMaybeArg handle t popRetr
      sessLoop handle env
    "DELE" -> do
      handleMaybeArg handle t popDele
      sessLoop handle env
    "QUIT" -> do
      handleNoArg handle popQuit
      return ()
    "NOOP" -> do
      handleNoArg handle popNoop
      sessLoop handle env
    "RSET" -> do
      handleNoArg handle popRset
      sessLoop handle env
    _ -> sendReply handle (POP3Reply ERR "command not supported") 

handleMaybeArg handle t func = do
  case maybeArg t of
        Nothing -> sendReply handle (POP3Reply ERR "no such message")
        Just t -> do
          let val = TR.decimal t
          case val of
            Left err -> sendReply handle (POP3Reply ERR $ T.pack err)
            Right (n, _) -> do
              either <- func n
              case either of
                Left err -> sendReply handle (POP3Reply ERR $ T.pack $ show err)
                Right s -> sendPreFormReply handle s
handleNoArg handle func = do
  either <- func
  case either of
    Left err -> sendReply handle (POP3Reply ERR $ T.pack $ show err)
    Right str -> sendPreFormReply handle str
    
{- | Store the passed in user in the MVar-}
handleUser handle cmd mvar = do
  let rhs = arg cmd
  putStrLn ("Supplied User is" ++ (T.unpack rhs))
  updateUser mvar rhs
  pop3OK handle
  
handlePass handle cmd mvar = do
  let rhs = arg cmd
  putStrLn ("Supplied Pass is" ++ (T.unpack rhs))
  updatePass mvar rhs
  pop3OK handle

sendPreFormReply handle reply = (hPutStr handle $ reply)

sendReply handle reply = (hPutStr handle $ show reply)

newEmptyPOP3MVar = atomically (newEmptyTMVar)

overwritePOP3Session :: TMVar a -> a -> IO ()
overwritePOP3Session mvar env = do
  atomically $ tryTakeTMVar mvar
  atomically $ (putTMVar mvar env)

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

pop3OK hndl = sendReply hndl $ POP3Reply OK ""
