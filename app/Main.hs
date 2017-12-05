module Main
  ( main
  ) where

import           Comms.Types
import           Comms.Common.Util
import           Comms.Eth.Scanner
import           Comms.Eth.Sender
import qualified Comms.POP3.Server             as POP3
import qualified Comms.SMTP.Server             as SMTP
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception.Base (bracket)
import           Control.Monad          (when)
import           Network
import           System.Console.CmdArgs
import           System.IO
import           System.Posix.Signals

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
  opts <- cmdArgsRun $ cmdArgsMode $ run
  let isDebug = getDebug opts
  config <- getConfigFromOptions opts
  let smtpPort = fromIntegral $ getSMTPPort config
  let pop3Port = fromIntegral $ getPOP3Port config
  when isDebug $ putStrLn $ show smtpPort
  when isDebug $ putStrLn $ show pop3Port
  (smtpSocket, pop3Socket) <- concurrently (listenOn $ PortNumber smtpPort) (listenOn $ PortNumber pop3Port)
  
  smtpFinished <- async (bindServer smtpSocket SMTP.handleConn config undefined)
  pop3Finished <- async (bindServer pop3Socket POP3.handleConn config undefined)
  when isDebug $ putStrLn "Listening on both sockets."
  
  let sigHandler = (serverSigHandler smtpFinished pop3Finished)
  installHandler keyboardSignal (Catch sigHandler) Nothing

  returnVals <- waitEitherCatch smtpFinished pop3Finished
  case returnVals of
    Left smtpEither -> case smtpEither of
                         Left excpt -> do
                           when isDebug $ putStrLn "Closing both sockets."
                           sClose smtpSocket
                           sClose pop3Socket
                         Right a -> return a
    Right pop3Either -> case pop3Either of
                          Left expt -> do
                            when isDebug $ putStrLn "Closing both sockets."
                            sClose pop3Socket
                            sClose smtpSocket
                          Right a -> return a
  when isDebug $ putStrLn "Server Shutting Down..."
{- TODO(broluwo): Write a unit test to ensure that the sockets are bound and can receive a request.
similar to `nc -vz 127.0.0.1 987` or `nc -vz 127.0.0.1 587`.
-}
-- | Close all the open sockets.
serverSigHandler asyncAction1 asyncAction2 = do
  putStrLn "\nSIGINT received. Cancelling all running threads..."
  cancel asyncAction1
  cancel asyncAction2
