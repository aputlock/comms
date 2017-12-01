{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Comms.SMTP.Handler
  ( datm
  , mail
  , rcpt
  , helo
  , noop
  , quit
  , rset
  , ok
  , sendReply
  ) where

import           Comms.Types
import           Comms.Common.Util
import qualified Comms.Eth.Sender       as Eth (sendEmail)
import           Comms.SMTP.State
import           Control.Concurrent.STM
import           Control.Monad          (when)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Network
import           System.IO              (Handle (..), hGetLine, hPutStr)
import           Text.RE.Replace
import           Text.RE.TDFA

{- | This value is how big we can let our message be in bytes.
Unfortunately this is directly related to the blockGasLimit
which is variable and expensive to calculate.-}
maxMessageSize :: Integer
maxMessageSize = 10240000

rcptToRE :: RE
rcptToRE = [re|[Tt][Oo]:\s*<(.+)>|]

mailFromRE :: RE
mailFromRE = [re|[Ff][Rr][Oo][Mm]:\s*<(.*)>|]

datm :: SMTPHandler
datm handle t env = do
  line <- hGetLine handle
  when (T.unpack (T.strip $ T.pack line) /= ".") $ do
    updateEnvContents env $ T.pack (line <> "\n")
    datm handle t env
  -- Make sure we have recipients already
  -- otherwise return error 554 5.5.1 Error: no valid recipients / need RCPT command

mail :: SMTPHandler
mail handle t env = do
  let rhs = arg t
  case captureTextMaybe [cp|1|] $ rhs ?=~ mailFromRE of
    Nothing -> sendReply handle $ Reply 501 "5.1.7 Bad sender address syntax"
    Just m -> do
      updateEnvFrom env m
      updateEnvState env HaveMailFrom
      sendReply handle $ Reply 250 "2.1.0 OK"
      -- Check the last state stored in the env. If it's not from HELO or earlier we need to abort with a nested MAIL Command err.

rcpt :: SMTPHandler
rcpt handle t env = do
  let rhs = arg t
  case captureTextMaybe [cp|1|] $ rhs ?=~ rcptToRE of
    Nothing -> sendReply handle $ Reply 501 "5.1.7 Bad sender address syntax"
    Just m -> do
      updateEnvTo env m
      updateEnvState env HaveRcptTo
      sendReply handle $ Reply 250 "2.1.0 OK"

helo :: SMTPHandler
helo handle cmd env = do
  putStrLn "Handling HELO"
  sendReply handle $ Reply 0 $ "250-comms Hello " <> arg cmd
  -- Now we need to send the extensions we support
  sendReply handle $ Reply 0 $ "250-SIZE " <> T.pack (show maxMessageSize)
  sendReply handle $ Reply 0 "250-ENHANCEDSTATUSCODES"
  sendReply handle $ Reply 0 "250-8BITMIME"
  case verb cmd of
    "HELO" -> updateEnvState env HaveHelo
    "EHLO" -> updateEnvState env HaveEhlo
  putStrLn "Handled HELO"

{- | Handler for `NOOP` command
Does nothing besides sending an OK status message-}
noop :: SMTPHandler
noop handle _ _ = ok handle

quit :: SMTPHandler
quit handle t env = sendReply handle $ Reply 221 "2.0.0 Bye"

{- | Resets the message that was being constructed to a blank state.
-}
rset :: SMTPHandler
rset handle _ env = do
  putStrLn "Inside RSET"
  overwriteEnvelope env (Envelope "" "" [] Unknown "")
  ok handle

sendReply :: Show a => Handle -> a -> IO ()
sendReply handle reply = hPutStr handle $ show reply

ok :: Handle -> IO ()
ok handle = sendReply handle $ Reply 250 "2.0.0 OK"
