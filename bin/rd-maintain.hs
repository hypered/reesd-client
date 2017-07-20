module Main (main) where

import System.Console.CmdArgs.Explicit (processArgs)
import System.Environment (lookupEnv)

import Reesd.Client.Commands.Maintain (maintainModes, processCmd)


------------------------------------------------------------------------------
main :: IO ()
main = do
  -- TODO Put this in a helper together with flagDomain
  mdomain <- lookupEnv "REESD_DOMAIN"
  case mdomain of
    Just domain -> putStrLn ("REESD_DOMAIN is set to " ++ domain ++ ".")
    _ -> return ()

  processArgs (maintainModes mdomain) >>= processCmd
