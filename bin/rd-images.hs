module Main (main) where

import System.Console.CmdArgs.Explicit (processArgs)

import Reesd.Client.Commands.Images (imagesModes, processCmd)


------------------------------------------------------------------------------
main :: IO ()
main = processArgs imagesModes >>= processCmd
