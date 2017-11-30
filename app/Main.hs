module Main
  ( main
  ) where

import           Comms.Common.Types
import           Comms.Common.Util
import           Comms.Eth.Scanner
import           Comms.Eth.Sender
import qualified Comms.POP3             as POP3
import qualified Comms.SMTP             as SMTP
import           Control.Concurrent
import           Control.Exception.Base (bracket)
import           Control.Monad          (when)
import           Network
import           System.Console.CmdArgs
import           System.IO
import           System.Signal

-- TODO(broluwo): Consider writing a generator function for creating the config file.
run =
  RunServerOptions
  { debug = def &= help "Print debugging info about the server"
  , config = def &= args &= typFile
  } &=
  program "Comms" &=
  details
    [ "Expects the location of the configuration file as an argument to the program."
    ] &=
  help "Decentralized email server." &=
  helpArg [explicit, name "h", name "help"] &=
  summary "Comms v0.0.1, (c) Brian Oluwo, Andrew Putlock"

commandMode = cmdArgsMode $ modes [run]

{-| General flow looks like, parse the command options bind the servers to their sockets and for every connection spin up a green thread to handle that session and re-register the handler to accept new connections.
-}
main :: IO ()
main = do
  children <- newMVar []
  opts <- cmdArgsRun $ cmdArgsMode $ run
  let isDebug = getDebug opts
  config <- getConfigFromOptions opts
  let smtpPort = fromIntegral $ getSMTPPort config
  let pop3Port = fromIntegral $ getPOP3Port config
  when isDebug $ putStrLn $ show smtpPort
  when isDebug $ putStrLn $ show pop3Port
  forkChild
    (bracket (listenOn $ PortNumber smtpPort) (sClose) $ \sock ->
       (bindServer sock SMTP.handleConn config undefined))
    children
  when isDebug $ putStrLn "Listening on SMTP socket"
  forkChild
    (bracket (listenOn $ PortNumber pop3Port) (sClose) $ \sock ->
       (bindServer sock POP3.handleConn config undefined))
    children
  when isDebug $ putStrLn "Listening on POP3 socket"
  when isDebug $ putStrLn "Bound to both sockets"
  when isDebug $ putStrLn $ show config
  waitForChildren children
  -- Want to do a check (if all threads are dead, continue. Need interrupt handler to also have access to the same channel so that when we receive an interrupt we can tell the respective server to suicide.

{- --Need to put more thought into how the signal handling will work.
  let sigHandler = (serverSigHandler (smtpSock : imapSock : []))
  installHandler sigINT sigHandler
  installHandler sigTERM sigHandler
-}
--  threadId <- forkIO (bracket (listenOn $ PortNumber smtpPort) (sClose) $ \sock -> (bindServer sock SMTP.handleConn config undefined))
--  threadId2 <- forkIO (bracket (listenOn $ PortNumber imapPort) (sClose) $ \socket -> (bindServer socket IMAP.handleConn config undefined))
{- TODO(broluwo): Write a unit test to ensure that the sockets are bound and can receive a request.
similar to `nc -vz 127.0.0.1 987` or `nc -vz 127.0.0.1 587`.
-}
-- | Close all the open sockets.
serverSigHandler :: [Socket] -> Signal -> IO ()
serverSigHandler socks _ = do
  putStrLn "Shutting down."
  last $ map (\sock -> sClose sock) socks

forkChild :: IO a -> MVar [MVar ()] -> IO ThreadId
forkChild io children = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar : childs)
  forkFinally io (\_ -> putMVar mvar ())

waitForChildren :: MVar [MVar a] -> IO ()
waitForChildren children = do
  cs <- takeMVar children
  case cs of
    [] -> do
      putStrLn "Shutting down servers..."
      return () -- All children are dead / or we have no children
    m:ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren children
