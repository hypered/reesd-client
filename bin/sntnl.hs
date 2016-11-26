{-# LANGUAGE RecordWildCards #-}
-- | `sntnl` is a Sentinel-specific version of `rd`.
module Main (main) where

import Data.Version (showVersion)
import Paths_reesd_client (version)
import System.Console.CmdArgs.Explicit

import qualified Reesd.Client.Commands.Sentinel as Sentinel


------------------------------------------------------------------------------
main :: IO ()
main = do
  cmd <- processArgs sentinelModes
  case cmd of
    Sentinel.Help ->
      print (helpText [] HelpFormatDefault sentinelModes)

    Sentinel.None -> do
      putStrLn versionString
      print (helpText [] HelpFormatDefault sentinelModes)

    _ -> Sentinel.processCmd cmd

sentinelModes :: Mode Sentinel.Cmd
sentinelModes = Sentinel.sentinelModes
  { modeNames = ["sntnl"]
  , modeHelp = "Command-line client for SNTNL."
  }

versionString :: String
versionString =
  "sntnl " ++ showVersion version ++ " - Copyright 2016 Hypered SPRL."


