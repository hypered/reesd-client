module Main (main) where

import System.Console.CmdArgs.Explicit (processArgs)

import Reesd.Client.Commands.Workflows (workflowsModes, processCmd)


------------------------------------------------------------------------------
main :: IO ()
main = processArgs workflowsModes >>= processCmd
