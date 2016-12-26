{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | cmdargs definitions for the `rd-sentinel` and `sntnl` executable.
module Reesd.Client.Commands.Sentinel where

import Data.List (intercalate)
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
processCmd Help = print (helpText [] HelpFormatDefault (sentinelModes Nothing))

processCmd Version = putStrLn versionString

processCmd None = do
  processCmd Version
  processCmd Help

processCmd CmdInstances{..} = call cmdDomain "workflows" "instances" ["--sentinel"]

processCmd CmdInstanciate{..} = call cmdDomain "workflows" "instanciate" args
  -- TODO Disallow + and other characters in the title.
  where title = intercalate "+" (words cmdTitle)
        args = (if null title then [] else ["--title", title]) ++ ["sentinel"]

processCmd CmdStep{..} = call cmdDomain "workflows" "step" args
  where
  args = ["--walk", show cmdWalkId, "--activity", "*wait-get"]

processCmd CmdDelete{..} = call cmdDomain "workflows" "delete" args
  where
  args = ["--walk", show cmdWalkId]


------------------------------------------------------------------------------
-- | Data type representing the different command-line subcommands.
data Cmd =
    CmdInstances { cmdDomain ::  String }
  | CmdInstanciate { cmdDomain :: String, cmdTitle :: String }
  | CmdStep { cmdDomain :: String, cmdWalkId :: Int }
  | CmdDelete { cmdDomain :: String, cmdWalkId :: Int }
  | Help
  | Version
  | None
  deriving Show

sentinelModes :: Maybe String -> Mode Cmd
sentinelModes mdomain = (modes "sentinel" None "Sentinels related subcommands."
  [ sentinelInstancesMode domain
  , sentinelInstanciateMode domain
  , sentinelStepMode domain
  , sentinelDeleteMode domain
  ])
  { modeGroupFlags = toGroup
    [ flagHelpSimple (const Help)
    , flagVersion (const Version)
    ]
  }
  where domain = maybe "sntnl.co" id mdomain

sentinelInstancesMode :: String -> Mode Cmd
sentinelInstancesMode domain = mode' "list" (sentinelInstances domain)
  "Display sentinels."
  [flagDomain]

sentinelInstanciateMode :: String -> Mode Cmd
sentinelInstanciateMode domain = mode "new" (sentinelInstanciate domain)
  "Create a new sentinel."
  (flagArg setTitle "TITLE")
  [flagDomain]
  where setTitle x r = Right (r { cmdTitle = x })

sentinelStepMode :: String -> Mode Cmd
sentinelStepMode domain = mode "poke" (sentinelStep domain)
  "Send a check-in to a sentinel."
  (flagArg setWalkId "ID")
  [flagDomain]
  where setWalkId x r = Right (r { cmdWalkId = read x }) -- TODO Left if not an ID.

sentinelDeleteMode :: String -> Mode Cmd
sentinelDeleteMode domain = mode "delete" (sentinelDelete domain)
  "Delete a sentinel."
  (flagArg setWalkId "ID")
  [flagDomain]
  where setWalkId x r = Right (r { cmdWalkId = read x }) -- TODO Left if not an ID.

sentinelInstances :: String -> Cmd
sentinelInstances domain = CmdInstances { cmdDomain = domain }

sentinelInstanciate :: String -> Cmd
sentinelInstanciate domain = CmdInstanciate { cmdDomain = domain, cmdTitle = "" }

sentinelStep :: String -> Cmd
sentinelStep domain = CmdStep { cmdDomain = domain, cmdWalkId = 0 }

sentinelDelete :: String -> Cmd
sentinelDelete domain = CmdDelete { cmdDomain = domain, cmdWalkId = 0 }


------------------------------------------------------------------------------
flagDomain = flagReq ["domain"]
  (\x r -> Right (r { cmdDomain = x }))
  "DOMAIN"
  "Domain to use, default to sntnl.co. Alternatively, set the SNTNL_DOMAIN \
  \environment variable."
