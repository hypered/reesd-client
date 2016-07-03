{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | cmdargs definitions for the `rd-users` executable.
module Reesd.Client.Commands.Users where

import Data.Version (showVersion)
import Paths_reesd_client (version)
import System.Console.CmdArgs.Explicit
  ( flagArg, flagHelpSimple, flagVersion, helpText, mode, modeArgs
  , modeEmpty, modeGroupFlags, modeHelp, modes, toGroup
  , Flag, Help, HelpFormat(..), Mode(..), Name
  )
import System.Process (createProcess, proc, waitForProcess)


------------------------------------------------------------------------------
-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "rd-users " ++ showVersion version ++ " - Copyright 2016 Hypered SPRL."


------------------------------------------------------------------------------
-- | Process the command-line choice.
processCmd :: Cmd -> IO ()
processCmd Help = print (helpText [] HelpFormatDefault usersModes)

processCmd Version = putStrLn versionString

processCmd None = do
  processCmd Version
  processCmd Help

processCmd CmdCreate{..} = callAdmin "create"

processCmd CmdStatus{..} = callAdmin "status"


------------------------------------------------------------------------------
-- | Data type representing the different command-line subcommands.
data Cmd =
    CmdList
  | CmdCreate { cmdLogin :: String }
  | CmdStatus
  | Help
  | Version
  | None
  deriving Show

usersModes :: Mode Cmd
usersModes = (modes "users" None "Users related subcommands."
  [ usersCreateMode
  , usersStatusMode
  ])
  { modeGroupFlags = toGroup
    [ flagHelpSimple (const Help)
    , flagVersion (const Version)
    ]
  }

usersCreateMode :: Mode Cmd
usersCreateMode = mode "create" usersCreate
  "Create a user."
  (flagArg setLogin "LOGIN")
  []
  where setLogin x r = Right (r { cmdLogin = x })

usersStatusMode :: Mode Cmd
usersStatusMode = mode' "status" usersStatus
  "Display the user status."
  []

usersCreate = CmdCreate ""

usersStatus = CmdStatus


------------------------------------------------------------------------------
-- | Call `reesd-admin` through SSH.
callAdmin command = do
  (_, _, _, h) <- createProcess
    (proc "ssh" ["rdadmin@reesd.dev", "reesd-admin", "users", command])
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
