module Main where

import           Comms.Common.Types
import           Comms.Common.Util
import           Comms.Eth.Scanner
import           Comms.Eth.Sender
import           Control.Monad
import           Data.Aeson             ((.:), (.=))
import qualified Data.Aeson             as JSON
import           System.Console.CmdArgs

-- TODO(broluwo): Consider writing a generator function for creating the config file.
-- TODO(broluwo): Document the format of the config file. It should map 1-to-1 with ServerConfig.
options =
  cmdArgsMode $
  Options
  { debug = def &= help "Print debugging info about the server"
  , config = def &= args &= typFile
  } &=
  program "Comms" &=
  details
    [ "Expects the location of the configuration file as an argument to the program."
    ] &=
  help "Decentralized email server." &=
  helpArg [explicit, name "h", name "help"] &=
  summary "Comms v0.0.0, (c) Brian Oluwo, Andrew Putlock"

main :: IO ()
main = print =<< getConfigFromOptions =<< cmdArgsRun options
