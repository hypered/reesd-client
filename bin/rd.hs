{-# LANGUAGE RecordWildCards #-}
-- | `rd` is a multi-call executable: it gathers all the `rd-xxx` executables
-- and exposes them as `xxx` subcommands. It can be used either directly as
-- `rd xxx` or throuh a symlink named `rd-xxx`.
module Main (main) where

import Data.Version (showVersion)
import Paths_reesd_client (version)
import System.Console.CmdArgs.Explicit
import System.Environment (getProgName, lookupEnv)

import qualified Reesd.Client.Commands.Maintain as Maintain
import qualified Reesd.Client.Commands.Images as Images
import qualified Reesd.Client.Commands.Users as Users
import qualified Reesd.Client.Commands.Workflows as Workflows


------------------------------------------------------------------------------
main :: IO ()
main = do
  mdomain <- lookupEnv "REESD_DOMAIN"
  case mdomain of
    Just domain -> putStrLn ("REESD_DOMAIN is set to " ++ domain ++ ".")
    _ -> return ()

  prog <- getProgName
  cmd <- case prog of
    "rd"            -> processArgs (allModes mdomain)
    "<interactive>" -> processArgs (allModes mdomain) -- For ghci.
    "rd-images"     -> processArgs imagesModes
    "rd-maintain"   -> processArgs (maintainModes mdomain) -- TODO Pass mdomain to all xxxModes.
    "rd-users"      -> processArgs usersModes
    "rd-workflows"  -> processArgs workflowsModes
    -- TODO Implement the following and rd-sentinel.hs.
    -- "rd-sentinel" -> processArgs sentinelModes
    _ -> return UnkownProgramName
  processCmd cmd


------------------------------------------------------------------------------
versionString :: String
versionString =
  "rd " ++ showVersion version ++ " - Copyright 2016 Hypered SPRL."

allModes :: Maybe String -> Mode Cmd
allModes mdomain = (modes "rd" None "Command-line client for Reesd."
  [ maintainModes mdomain -- TODO Display in --help only if some configuration says admin=yes.
  , imagesModes
  , usersModes -- TODO Display in --help only if some configuration says admin=yes.
  , workflowsModes
  ])
  { modeGroupFlags = toGroup
    [ flagHelpSimple (const Help)
    , flagVersion (const Version)
    ]
  }


------------------------------------------------------------------------------
-- | Process the command-line choice.
processCmd :: Cmd -> IO ()
processCmd (Maintain cmd) = Maintain.processCmd cmd
processCmd (Images cmd) = Images.processCmd cmd
processCmd (Users cmd) = Users.processCmd cmd
processCmd (Workflows cmd) = Workflows.processCmd cmd

processCmd Help = print (helpText [] HelpFormatDefault (allModes Nothing))

processCmd Version = putStrLn versionString

processCmd None = do
  processCmd Version
  processCmd Help

processCmd UnkownProgramName = do
  putStrLn "The rd executable is called with an unknown program name."
  putStrLn "Only program names matching rd subcommands are recognized."


------------------------------------------------------------------------------
-- | Available commands.
data Cmd =
    Maintain { getMaintain :: Maintain.Cmd }
  | Images { getImages :: Images.Cmd }
  | Users { getUsers :: Users.Cmd }
  | Workflows { getWorkflows :: Workflows.Cmd }
  | Help
  | Version
  | None
  | UnkownProgramName
  deriving Show

maintainModes :: Maybe String -> Mode Cmd
maintainModes mdomain =
  remap2 Maintain getMaintain (Maintain.maintainModes mdomain)
  { modeNames = ["maintain"] }

imagesModes :: Mode Cmd
imagesModes = remap2 Images getImages Images.imagesModes { modeNames = ["images"] }

usersModes :: Mode Cmd
usersModes = remap2 Users getUsers Users.usersModes { modeNames = ["users"] }

workflowsModes :: Mode Cmd
workflowsModes = remap2 Workflows getWorkflows Workflows.workflowsModes { modeNames = ["workflows"] }
