{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | cmdargs definitions for the `rd-maintain` executable.
module Reesd.Client.Commands.Maintain where

import Data.Version (showVersion)
import Paths_reesd_client (version)
import System.Console.CmdArgs.Explicit
  ( flagHelpSimple, flagReq, flagVersion, helpText
  , modeGroupFlags, modes, toGroup
  , Flag, HelpFormat(..), Mode(..)
  )

import Reesd.Client.Helpers (callAdmin, mode')


------------------------------------------------------------------------------
-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "rd-maintain " ++ showVersion version ++ " - Copyright 2017 Hypered SPRL."


------------------------------------------------------------------------------
-- | Process the command-line choice.
processCmd :: Cmd -> IO ()
processCmd Help = print (helpText [] HelpFormatDefault (maintainModes Nothing))

processCmd Version = putStrLn versionString

processCmd None = do
  processCmd Version
  processCmd Help

processCmd CmdPruneSentinels{..} = do
  callAdmin cmdDomain "maintain" "prune-sentinels" []

processCmd CmdOverview{..} = callAdmin cmdDomain "maintain" "overview" []


------------------------------------------------------------------------------
-- | Data type representing the different command-line subcommands.
data Cmd =
    CmdPruneSentinels
    { cmdDomain :: String
    }
  | CmdOverview
    { cmdDomain :: String
    }
  | Help
  | Version
  | None
  deriving Show

maintainModes :: Maybe String -> Mode Cmd
maintainModes mdomain =
  (modes "maintain" None "Maintenance related subcommands."
    [ maintainPruneSentinelsMode domain
    , maintainOverviewMode domain
    ])
  { modeGroupFlags = toGroup
    [ flagHelpSimple (const Help)
    , flagVersion (const Version)
    ]
  }
  where domain = maybe "reesd.com" id mdomain

maintainPruneSentinelsMode :: String -> Mode Cmd
maintainPruneSentinelsMode domain =
  mode' "prune-sentinels" (maintainPruneSentinels domain)
    "PruneSentinels a user."
    [flagDomain]

maintainOverviewMode :: String -> Mode Cmd
maintainOverviewMode domain = mode' "overview" (maintainOverview domain)
  "Display the user overview."
  [flagDomain]

maintainPruneSentinels :: String -> Cmd
maintainPruneSentinels domain = CmdPruneSentinels { cmdDomain = domain }

maintainOverview :: String -> Cmd
maintainOverview domain = CmdOverview { cmdDomain = domain }


------------------------------------------------------------------------------
flagDomain :: Flag Cmd
flagDomain = flagReq ["domain"]
  (\x r -> Right (r { cmdDomain = x }))
  "DOMAIN"
  "Domain to use, default to reesd.com. Alternatively, set the REESD_DOMAIN \
  \environment variable."
