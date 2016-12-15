{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | cmdargs definitions for the `rd-users` executable.
module Reesd.Client.Commands.Users where

import Data.Version (showVersion)
import Paths_reesd_client (version)
import System.Console.CmdArgs.Explicit
  ( flagHelpSimple, flagReq, flagVersion, helpText
  , modeGroupFlags, modes, toGroup
  , HelpFormat(..), Mode(..)
  )

import Reesd.Client.Helpers (call, call', mode')


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

processCmd CmdCreate{..} = do
  case cmdPublicKeyPath of
    Nothing -> call "reesd.dev" "users" "create" ["--login", cmdLogin, "--email", cmdEmail]
    Just path -> do
      content <- readFile path
      call' "reesd.dev" "users" "create" ["--login", cmdLogin, "--email", cmdEmail, "--public-key", "-"] content

processCmd CmdStatus{..} = call "reesd.dev" "users" "status" []


------------------------------------------------------------------------------
-- | Data type representing the different command-line subcommands.
data Cmd =
    CmdCreate
    { cmdLogin :: String
    , cmdEmail :: String
    , cmdPublicKeyPath :: Maybe String
    }
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
usersCreateMode = mode' "create" usersCreate
  "Create a user."
  [flagReq ["login"]
      (\x r -> Right (r { cmdLogin = x }))
      "STRING"
      "User login"
  , flagReq ["email"]
      (\x r -> Right (r { cmdEmail = x }))
      "STRING"
      "User email."
  , flagReq ["public-key"]
      (\x r -> Right (r { cmdPublicKeyPath = Just x }))
      "PATH"
      "Path to a public SSH key."
  ]

usersStatusMode :: Mode Cmd
usersStatusMode = mode' "status" usersStatus
  "Display the user status."
  []

usersCreate :: Cmd
usersCreate = CmdCreate "" "" Nothing

usersStatus :: Cmd
usersStatus = CmdStatus
