{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | cmdargs definitions for the `rd-sentinel` and `sntnl` executable.
module Reesd.Client.Commands.Sentinel where

import Data.Version (showVersion)
import Paths_reesd_client (version)
import System.Console.CmdArgs.Explicit
  ( flagArg, flagHelpSimple, flagReq, flagVersion, helpText, mode
  , modeGroupFlags, modes, toGroup
  , HelpFormat(..), Mode(..)
  )

import Reesd.Client.Helpers (call, mode')


------------------------------------------------------------------------------
-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "rd-sentinel " ++ showVersion version ++ " - Copyright 2016 Hypered SPRL."


------------------------------------------------------------------------------
-- | Process the command-line choice.
processCmd :: Cmd -> IO ()
processCmd Help = print (helpText [] HelpFormatDefault sentinelModes)

processCmd Version = putStrLn versionString

processCmd None = do
  processCmd Version
  processCmd Help

processCmd CmdInstances{..} = call "workflows" "instances" ["--sentinel"]

processCmd CmdInstanciate{..} = call "workflows" "instanciate" ["--title", cmdTitle, "sentinel"] -- TODO Multi-words titles are broken: only first word is in db.

processCmd CmdStep{..} = call "workflows" "step" args
  where
  -- TODO Use the secret string instead of an ID.
  args = ["--walk", show cmdWalkId, "--activity", "wait-get"]


------------------------------------------------------------------------------
-- | Data type representing the different command-line subcommands.
data Cmd =
    CmdInstances
  | CmdInstanciate { cmdTitle :: String }
  | CmdStep { cmdWalkId :: Int }
  | Help
  | Version
  | None
  deriving Show

sentinelModes :: Mode Cmd
sentinelModes = (modes "sentinel" None "Sentinels related subcommands."
  [ sentinelInstancesMode
  , sentinelInstanciateMode
  , sentinelStepMode
  ])
  { modeGroupFlags = toGroup
    [ flagHelpSimple (const Help)
    , flagVersion (const Version)
    ]
  }

sentinelInstancesMode :: Mode Cmd
sentinelInstancesMode = mode' "list" sentinelInstances
  "Display sentinels."
  []

sentinelInstanciateMode :: Mode Cmd
sentinelInstanciateMode = mode "new" sentinelInstanciate
  "Create a new sentinel."
  (flagArg setTitle "TITLE")
  []
  where setTitle x r = Right (r { cmdTitle = x })

sentinelStepMode :: Mode Cmd
sentinelStepMode = mode "poke" sentinelStep
  "Send a check-in to a sentinel."
  (flagArg setWalkId "ID")
  []
  where setWalkId x r = Right (r { cmdWalkId = read x }) -- TODO Left if not an ID.

sentinelInstances :: Cmd
sentinelInstances = CmdInstances

sentinelInstanciate :: Cmd
sentinelInstanciate = CmdInstanciate { cmdTitle = "" }

sentinelStep :: Cmd
sentinelStep = CmdStep { cmdWalkId = 0 }
