{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | cmdargs definitions for the `rd-images` executable.
module Reesd.Client.Commands.Images where

import Data.Version (showVersion)
import Paths_reesd_client (version)
import System.Console.CmdArgs.Explicit
  ( flagHelpSimple, flagVersion, helpText, modeGroupFlags
  , modes, toGroup
  , HelpFormat(..), Mode(..)
  )

import Reesd.Client.Helpers (call, mode')


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

processCmd CmdList{..} = call "reesd.dev" "images" "list" []

processCmd CmdStatus{..} = call "reesd.dev" "images" "status" []


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

imagesList :: Cmd
imagesList = CmdList

imagesStatus :: Cmd
imagesStatus = CmdStatus
