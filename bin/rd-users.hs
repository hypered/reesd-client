module Main (main) where

import System.Console.CmdArgs.Explicit (processArgs)

import Reesd.Client.Commands.Users (usersModes, processCmd)


------------------------------------------------------------------------------
main :: IO ()
main = processArgs usersModes >>= processCmd
