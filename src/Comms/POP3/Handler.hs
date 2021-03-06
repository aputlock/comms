{-# LANGUAGE OverloadedStrings #-}

module Comms.POP3.Handler
  ( capa
  , Comms.POP3.Handler.user
  , Comms.POP3.Handler.pass
  , stat
  , list
  , uidl
  , retr
  , dele
  , quit
  , noop
  , rset
  , sendReply
  ) where

import           Comms.Common.Util
import           Comms.Eth.Scanner
import           Comms.POP3.State
import           Comms.Types

import           Control.Concurrent.STM
import           Control.Monad          (forM_)
import qualified Data.Text              as T
import qualified Data.Text.Read         as TR
import           Network
import           System.IO

capa :: POP3Handler
capa handle t mvar inboxState = do
  sendReply handle $ POP3Reply OK "Capability list follows"
  sendPreFormReply handle "USER\r\n"
  sendPreFormReply handle "UIDL\r\n"
  sendPreFormReply handle ".\r\n"

{- | Store the passed in user in the MVar-}
user :: POP3Handler
user handle cmd mvar inboxState = do
  let rhs = arg cmd
  putStrLn ("Supplied User is " ++ T.unpack rhs)
  updateUser mvar rhs
  pop3OK handle

pass :: POP3Handler
pass handle cmd mvar inboxState = do
  let rhs = arg cmd
  putStrLn ("Supplied Pass is " ++ T.unpack rhs)
  updatePass mvar rhs
  pop3OK handle

stat :: POP3Handler
stat handle t mvar inboxState = handleNoArg handle $ popStat inboxState

list :: POP3Handler
list handle t mvar inboxState =
  case maybeArg t of
    Nothing -> do
      either <- popList inboxState Nothing
      case either of
        Left err -> sendReply handle (POP3Reply ERR $ T.pack $ show err)
        Right s  -> forM_ s $ sendPreFormReply handle
    Just parsed -> do
      let eitherNum = TR.decimal parsed
      case eitherNum of
        Left err -> sendReply handle (POP3Reply ERR $ T.pack $ show err)
        Right (num, _) -> do
          e <- popList inboxState $ Just num
          case e of
            Left err -> sendReply handle (POP3Reply ERR $ T.pack $ show err)
            Right s  -> forM_ s $ sendPreFormReply handle

uidl :: POP3Handler
uidl handle t mvar inboxState =
  case maybeArg t of
    Nothing -> do
      either <- popUidl inboxState Nothing
      case either of
        Left err -> sendReply handle (POP3Reply ERR $ T.pack $ show err)
        Right s  -> forM_ s $ sendPreFormReply handle
    Just parsed -> do
      let eitherNum = TR.decimal parsed
      case eitherNum of
        Left err -> sendReply handle (POP3Reply ERR $ T.pack $ show err)
        Right (num, _) -> do
          e <- popUidl inboxState $ Just num
          case e of
            Left err -> sendReply handle (POP3Reply ERR $ T.pack $ show err)
            Right s  -> forM_ s $ sendPreFormReply handle

retr :: POP3Handler
retr handle t mvar inboxState = handleMaybeArg handle t $ popRetr inboxState

dele :: POP3Handler
dele handle t mvar inboxState = handleMaybeArg handle t $ popDele inboxState

quit :: POP3Handler
quit handle t mvar inboxState = handleNoArg handle $ popQuit inboxState

noop :: POP3Handler
noop handle t mvar inboxState = handleNoArg handle $ popNoop inboxState

rset :: POP3Handler
rset handle t mvar inboxState = handleNoArg handle $ popRset inboxState

sendPreFormReply = hPutStr

sendReply handle reply = hPutStr handle $ show reply

pop3OK hndl = sendReply hndl $ POP3Reply OK ""

handleMaybeArg ::
     (Integral t, Show a)
  => Handle
  -> T.Text
  -> (t -> IO (Either a String))
  -> IO ()
handleMaybeArg handle t func =
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
            Right s  -> sendPreFormReply handle s

handleNoArg :: Show a => Handle -> IO (Either a String) -> IO ()
handleNoArg handle func = do
  either <- func
  case either of
    Left err  -> sendReply handle (POP3Reply ERR $ T.pack $ show err)
    Right str -> sendPreFormReply handle str
