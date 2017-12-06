{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import           Comms.Types
import           Comms.Common.Util
import qualified Comms.POP3.Server             as POP3
import qualified Comms.SMTP.Server             as SMTP
import qualified Comms.Eth.AddressBook         as AB (importContact)
import qualified Comms.Eth.Sender              as S (sendContactCard)
import           System.Console.CmdArgs
import           System.Environment
import qualified Data.Text                     as T
-- TODO(broluwo): Consider writing a generator function for creating the config file.
run =
  RunServerOptions
  { debug = def &= help "Print debugging info about the server"
  } &= help "Runs the servers."
  &= explicit &= name "run"

importContact = ImportContact
  { email = def &= typ "EMAIL" &= argPos 0
  , hash = def &= typ "HASH" &= argPos 1
  } &= help "Import a contact into a local only address book." &= explicit &= name "import"

publishContact = PublishContact
  {} &= help "Publish contact card onto the public transaction log." &= explicit &= name "publish"

commandMode = cmdArgsMode $ modes [run &= auto, importContact, publishContact] &=
  program "comms" &=
  help "Decentralized email server." &=
  helpArg [explicit, name "h", name "help"] &=
  summary "Comms v0.0.1, (c) Brian Oluwo, Andrew Putlock"

{-| General flow looks like, parse the command options bind the servers to their sockets and for every connection spin up a green thread to handle that session and re-register the handler to accept new connections.
-}
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Please use the -h flag on the executable to see the usage text."
    _ -> do
      opts <- cmdArgsRun $ commandMode
      case opts of
        RunServerOptions debug -> do
          config <- getDefaultConfig
          runServer debug config SMTP.handleConn POP3.handleConn
        ImportContact email hash -> do
          AB.importContact email $ T.pack hash
          putStrLn "Imported contact."
        PublishContact -> do
          maybeHash <- S.sendContactCard
          case maybeHash of
            Left err -> putStrLn $ show err
            Right hash -> putStrLn $ "Posted contact card: " ++ T.unpack hash
