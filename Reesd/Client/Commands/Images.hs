{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | cmdargs definitions for the `rd-images` executable.
module Reesd.Client.Commands.Images where

import Data.Version (showVersion)
import Paths_reesd_client (version)
import System.Console.CmdArgs.Explicit
  ( flagHelpSimple, flagVersion, helpText, modeArgs, modeEmpty, modeGroupFlags
  , modeHelp, modes, toGroup
  , Flag, Help, HelpFormat(..), Mode(..), Name
  )
import System.Process (createProcess, proc, waitForProcess)


------------------------------------------------------------------------------
-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "rd-images " ++ showVersion version ++ " - Copyright 2016 Hypered SPRL."


------------------------------------------------------------------------------
-- | Process the command-line choice.
processCmd :: Cmd -> IO ()
processCmd Help = print (helpText [] HelpFormatDefault imagesModes)

processCmd Version = putStrLn versionString

processCmd None = do
  processCmd Version
  processCmd Help

processCmd cmd = runCmd cmd


------------------------------------------------------------------------------
-- | Data type representing the different command-line subcommands.
data Cmd =
    CmdList
  | CmdStatus
  | Help
  | Version
  | None
  deriving Show

imagesModes :: Mode Cmd
imagesModes = (modes "images" None "Docker repositories related subcommands."
  [ imagesListMode
  , imagesStatusMode
  ])
  { modeGroupFlags = toGroup
    [ flagHelpSimple (const Help)
    , flagVersion (const Version)
    ]
  }

imagesListMode :: Mode Cmd
imagesListMode = mode' "list" imagesList
  "Display a list of repositories."
  []

imagesStatusMode :: Mode Cmd
imagesStatusMode = mode' "status" imagesStatus
  "Display a list of repositories being updated."
  []

imagesList = CmdList

imagesStatus = CmdStatus

runCmd :: Cmd -> IO ()
runCmd CmdList{..} = call "list"
runCmd CmdStatus{..} = call "status"


------------------------------------------------------------------------------
-- | Call `reesd-command` through SSH.
call command = do
  (_, _, _, h) <- createProcess
    (proc "ssh" ["rd@reesd.dev", "reesd-command", "images", command])
  waitForProcess h
  return ()



------------------------------------------------------------------------------
-- | Same as `mode` but without an `Arg a` argument.
mode' :: Name -> a -> Help -> [Flag a] -> Mode a
mode' name value help flags = (modeEmpty value)
  { modeNames = [name]
  , modeHelp = help
  , modeArgs = ([], Nothing)
  , modeGroupFlags = toGroup flags
  }
