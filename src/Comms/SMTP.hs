{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Comms.SMTP
  ( handleConn
  ) where

import           Comms.Common.Types
import           Comms.Eth.Sender
import           Control.Concurrent.STM
import           Control.Monad          (when)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Network
import           System.IO
import           Text.RE.Replace
import           Text.RE.TDFA

-- TODO(broluwo): We can have this passed in
hostName = "127.0.0.1"

maxMessageSize = 10240000

rcptToRE = [re|[Tt][Oo]:\s*<(.+)>|]

mailFromRE = [re|[Ff][Rr][Oo][Mm]:\s*<(.*)>|]

{-|
There is a loop here as well until we hit a quit command
-}
handleConn :: Handle -> Config -> t -> IO ()
handleConn handle config channel = do
  startSession handle
  putStrLn "Returning from thread"
  -- After parsing it into some kind of data or record
  -- do a pattern match on that plus the previous state to see what
  -- the next step should be.

--  hClose handle
--  putStrLn "Closed handle"
startSession :: Handle -> IO ()
startSession handle = do
  putStrLn "In startSession"
  env <- newEmptyEnvelopeMVar
  sendReply handle newConn
  sessLoop handle env

sessLoop :: Handle -> EnvelopeMVar -> IO ()
sessLoop handle env = do
  putStrLn "Top of sessLoop"
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
      putStrLn "Handling QUIT"
      quit handle t env
      return ()
    "RSET" -> do
      putStrLn "-=-Handling RSET-=- "
      rset handle t env
      putStrLn "-=-Handled RSET-=-"
      sessLoop handle env
    "NOOP" -> do
      putStrLn "-=-Handling NOOP-=-"
      noop handle t env
      putStrLn "-=-Handled NOOP-=-"
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
      putStrLn $ show e
      -- Message is ready to send off at this point, pass to ethereum
      -- Then clear the envelopemvar, and say you're ready for a new
      -- "session".
      let resp = fmap (\recip -> sendEmail (T.unpack recip) (T.unpack $ contents e)) $ to e
          strs = fmap (\io -> do
                         r <- io
                         return $ case r of
                                    Left err -> show err
                                    Right hash -> "Message sent: " ++ T.unpack hash
                      ) resp
      foldr (\io acc -> do
              str <- io
              putStrLn str
           ) (return ()) strs
      ok handle
      sessLoop handle env
    _ -> do
      sendReply handle (Reply 502 "5.5.2 Error: command not recognized")
      sessLoop handle env



datm handle t env = do
  line <- hGetLine handle
  when ((T.unpack $ T.strip $ T.pack line) /= ".") $ do
    updateEnvContents env $ T.pack (line <> "\n")
      -- Update Data
    datm handle t env
  -- Make sure we have recipients already
  -- otherwise return error 554 5.5.1 Error: no valid recipients / need RCPT command

sendReply handle reply = (hPutStr handle $ show reply)

newConn = Reply 220 (hostName <> " SMTP Service Ready comms")

{-| Get the string representation of the requested command -}
verb :: T.Text -> T.Text
verb str = T.strip $ head $ T.splitOn crlf $ head $ T.words str

arg :: T.Text -> T.Text
arg str = T.stripEnd $ head $ T.splitOn crlf $ T.unwords $ tail $ T.words str

crlf :: T.Text
crlf = "\r\n"

mail handle t env = do
  let rhs = arg t
  case captureTextMaybe [cp|1|] $ rhs ?=~ mailFromRE of
    Nothing -> sendReply handle $ Reply 501 "5.1.7 Bad sender address syntax"
    Just m -> do
      updateEnvFrom env m
      updateEnvState env HaveMailFrom
      sendReply handle $ Reply 250 "2.1.0 OK"
      -- Check the last state stored in the env. If it's not from HELO or earlier we need to abort with a nested MAIL Command err.

rcpt handle t env = do
  let rhs = arg t
  case captureTextMaybe [cp|1|] $ rhs ?=~ rcptToRE of
    Nothing -> sendReply handle $ Reply 501 "5.1.7 Bad sender address syntax"
    Just m -> do
      updateEnvTo env m
      updateEnvState env HaveRcptTo
      sendReply handle $ Reply 250 "2.1.0 OK"

helo :: Handle -> T.Text -> EnvelopeMVar -> IO ()
helo handle cmd env = do
  putStrLn "Handling HELO"
  sendReply handle $ Reply 0 ("250-" <> hostName <> " Hello " <> (arg cmd))
  -- Now we need to send the extensions we support
  sendReply handle $ Reply 0 ("250-SIZE " <> (T.pack $ show maxMessageSize))
  sendReply handle $ Reply 0 ("250-ENHANCEDSTATUSCODES")
  sendReply handle $ Reply 0 ("250-8BITMIME")
  sendReply handle $ Reply 250 "DSN"
  case verb cmd of
    "HELO" -> updateEnvState env HaveHelo
    "EHLO" -> updateEnvState env HaveEhlo
  putStrLn "Handled HELO"

{- | Handler for `NOOP` command
Does nothing besides sending an OK status message-}
noop :: Handle -> t -> t1 -> IO ()
noop handle _ _ = ok handle

quit handle t env = sendReply handle $ Reply 221 "2.0.0 Bye"

{- | Handler for `RSET` command
Resets the message that was being constructed to a blank state.
-}
rset :: Handle -> t -> EnvelopeMVar -> IO ()
rset handle _ env = do
  putStrLn "Inside RSET"
  overwriteEnvelope env (Envelope "" "" [] Unknown "")
  ok handle

newEmptyEnvelopeMVar = atomically (newEmptyTMVar)

{- | Swaps the value -}
updateEnvelope :: TMVar a -> a -> IO ()
updateEnvelope env = atomically . putTMVar env

{- | Replaces contents of the TMVar with the supplied value.
Non-Blocking.
-}
overwriteEnvelope :: TMVar a -> a -> IO ()
overwriteEnvelope mvar env = do
  old <- atomically $ tryTakeTMVar mvar
  case old of
    Nothing -> atomically $ (putTMVar mvar env)
    Just _  -> atomically $ (putTMVar mvar env)

getEnvelope = atomically . takeTMVar

ok :: Handle -> IO ()
ok handle = sendReply handle $ Reply 250 "2.0.0 OK"

updateEnvState mvar newState = do
  putStrLn "Entered updateEnvState"
  old <- atomically $ tryTakeTMVar mvar
  putStrLn "updateEnvState - Took TMVar"
  case old of
    Nothing -> do
      putStrLn "updateEnvState - Nothing Case."
      overwriteEnvelope mvar (Envelope "" "" [] newState "")
    Just oldE -> do
      putStrLn "updateEnvState - Just Case."
      overwriteEnvelope mvar (oldE {state = newState})

updateEnvFrom :: TMVar Envelope -> T.Text -> IO ()
updateEnvFrom mvar from = do
  putStrLn "Entered updateEnvFrom"
  old <- atomically $ tryTakeTMVar mvar
  putStrLn "updateEnvFrom - Took TMVar"
  case old of
    Nothing -> do
      putStrLn "updateEnvFrom - Nothing Case."
      overwriteEnvelope mvar (Envelope "" from [] HaveMailFrom "")
    Just oldE -> do
      putStrLn "updateEnvFrom - Just Case."
      overwriteEnvelope mvar (oldE {from = from})

updateEnvTo :: TMVar Envelope -> T.Text -> IO ()
updateEnvTo mvar newTo = do
  putStrLn "Entered updateEnvTo"
  old <- atomically $ tryTakeTMVar mvar
  putStrLn "updateEnvTo - Took TMVar"
  case old of
    Nothing -> do
      putStrLn "updateEnvTo - Nothing Case."
      overwriteEnvelope mvar (Envelope "" "" [newTo] HaveRcptTo "")
    Just oldE -> do
      putStrLn "updateEnvTo - Just Case."
      overwriteEnvelope mvar (oldE {to = newTo : (to oldE)})

updateEnvContents mvar newContents = do
  putStrLn "Entered updateEnvContents"
  old <- atomically $ tryTakeTMVar mvar
  putStrLn "updateEnvContents - Took TMVar"
  case old of
    Nothing -> do
      putStrLn "updateEnvContents - Nothing Case."
      overwriteEnvelope mvar (Envelope "" "" [] HaveRcptTo newContents)
    Just oldE -> do
      putStrLn "updateEnvContents - Just Case."
      overwriteEnvelope mvar (oldE {contents = (contents oldE) <> newContents})
