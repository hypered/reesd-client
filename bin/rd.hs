{-# LANGUAGE RecordWildCards #-}
-- | `rd` is a multi-call executable: it gathers all the `rd-xxx` executables
-- and exposes them as `xxx` subcommands. It can be used either directly as
-- `rd xxx` or throuh a symlink named `rd-xxx`.
module Main (main) where

import Data.Version (showVersion)
import Paths_reesd_client (version)
import System.Console.CmdArgs.Explicit
import System.Environment (getProgName)

import qualified Reesd.Client.Commands.Images as Images


------------------------------------------------------------------------------
main :: IO ()
main = do
  prog <- getProgName
  cmd <- case prog of
    "rd"            -> processArgs allModes
    "<interactive>" -> processArgs allModes      -- For ghci.
    "rd-images"     -> processArgs imagesModes
    _ -> return UnkownProgramName
  processCmd cmd


------------------------------------------------------------------------------
versionString :: String
versionString =
  "rd " ++ showVersion version ++ " - Copyright 2016 Hypered SPRL."

allModes = (modes "rd" None "Command-line client for Reesd."
  [ imagesModes
  ])
  { modeGroupFlags = toGroup
    [ flagHelpSimple (const Help)
    , flagVersion (const Version)
    ]
  }


------------------------------------------------------------------------------
-- | Process the command-line choice.
processCmd :: Cmd -> IO ()
processCmd (Images cmd) = Images.processCmd cmd

processCmd Help = print (helpText [] HelpFormatDefault allModes)

processCmd Version = do
  putStrLn versionString
  print (helpText [] HelpFormatDefault allModes)

processCmd None = do
  processCmd Version
  processCmd Help

processCmd UnkownProgramName = do
  putStrLn "The rd executable is called with an unknown program name."
  putStrLn "Only program names matching rd subcommands are recognized."


------------------------------------------------------------------------------
-- | Available commands.
data Cmd =
    Images { getImages :: Images.Cmd }
  | Help
  | Version
  | None
  | UnkownProgramName
  deriving Show

imagesModes :: Mode Cmd
imagesModes = remap2 Images getImages Images.imagesModes { modeNames = ["images"] }
