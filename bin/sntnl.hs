{-# LANGUAGE RecordWildCards #-}
-- | `sntnl` is a Sentinel-specific version of `rd`.
module Main (main) where

import Data.Version (showVersion)
import Paths_reesd_client (version)
import System.Console.CmdArgs.Explicit
import System.Environment (lookupEnv)

import qualified Reesd.Client.Commands.Sentinel as Sentinel


------------------------------------------------------------------------------
main :: IO ()
main = do
  mdomain <- lookupEnv "SNTNL_DOMAIN"
  case mdomain of
    Just domain -> putStrLn ("SNTNL_DOMAIN is set to " ++ domain ++ ".")
    _ -> return ()

  cmd <- processArgs (sentinelModes mdomain)
  case cmd of
    Sentinel.Help ->
      print (helpText [] HelpFormatDefault (sentinelModes mdomain))

    Sentinel.None -> do
      putStrLn versionString
      print (helpText [] HelpFormatDefault (sentinelModes mdomain))

    _ -> Sentinel.processCmd cmd

sentinelModes :: Maybe String -> Mode Sentinel.Cmd
sentinelModes mdomain = (Sentinel.sentinelModes mdomain)
  { modeNames = ["sntnl"]
  , modeHelp = "Command-line client for SNTNL."
  }

versionString :: String
versionString =
  "sntnl " ++ showVersion version ++ " - Copyright 2016 Hypered SPRL."


