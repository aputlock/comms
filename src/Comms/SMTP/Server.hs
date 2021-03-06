{-# LANGUAGE OverloadedStrings #-}

module Comms.SMTP.Server
  ( handleConn
  ) where

import           Comms.Common.Util
import qualified Comms.Eth.Sender       as Eth (sendEmail)
import           Comms.SMTP.Handler
import           Comms.SMTP.State
import           Comms.Types
import           Control.Concurrent.STM
import           Control.Monad          (forM_, when)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Network
import           System.IO              (Handle (..), hGetLine, hPutStr)
import           Text.RE.Replace
import           Text.RE.TDFA

-- TODO(broluwo): We can have this passed in
hostName :: T.Text
hostName = "127.0.0.1"

handleConn :: Handle -> Config -> t -> IO ()
handleConn handle config channel = do
  startSession handle
  putStrLn "-=-=-=- Closing SMTP Session -=-=-=-"

startSession :: Handle -> IO ()
startSession handle = do
  putStrLn "-=-=-=- Starting SMTP Session -=-=-=-"
  env <- newEmptyEnvelopeMVar
  sendReply handle newConn
  sessLoop handle env

sessLoop :: Handle -> EnvelopeMVar -> IO ()
sessLoop handle env = do
  line <- hGetLine handle
  let t = T.pack line
  case verb t of
    "HELO" -> do
      helo handle t env
      sessLoop handle env
    "EHLO" -> do
      helo handle t env
      sessLoop handle env
    "QUIT" -> do
      quit handle t env
      return ()
    "RSET" -> do
      rset handle t env
      sessLoop handle env
    "NOOP" -> do
      noop handle t env
      sessLoop handle env
    "MAIL" -> do
      mail handle t env
      sessLoop handle env
    "RCPT" -> do
      rcpt handle t env
      sessLoop handle env
    "DATA" -> do
      sendReply handle $ Reply 354 "OK. Go ahead. End with <CRLF>.<CRLF>"
      datm handle t env
      updateEnvState env HaveData
      e <- atomically $ readTMVar env
      print e
      -- Message is ready to send off at this point, pass to ethereum
      sendEmail e
      -- Then clear the envelopemvar, and say you're ready for a new "session".
      ok handle
      sessLoop handle env
    _ -> do
      sendReply handle (Reply 502 "5.5.2 Error: command not recognized")
      sessLoop handle env

newConn :: SMTPReply
newConn = Reply 220 (hostName <> " SMTP Service Ready comms")

sendEmail :: Envelope -> IO ()
sendEmail e = do
  let resp =
        fmap (\recip -> Eth.sendEmail (T.unpack recip) (T.unpack $ contents e)) $
        to e
  let strs =
        fmap
          (\io -> do
             r <- io
             return $
               case r of
                 Left err   -> show err
                 Right hash -> "Message sent: " ++ T.unpack hash)
          resp
  forM_ strs (>>= putStrLn)
