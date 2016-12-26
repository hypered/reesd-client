{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | cmdargs definitions for the `rd-workflows` executable.
module Reesd.Client.Commands.Workflows where

import Data.Version (showVersion)
import Paths_reesd_client (version)
import System.Console.CmdArgs.Explicit
  ( flagArg, flagBool, flagHelpSimple, flagReq, flagVersion, helpText, mode
  , modeGroupFlags, modes, toGroup
  , HelpFormat(..), Mode(..)
  )

import Reesd.Client.Helpers (call, call', callFor, callFor', forUserFlag, mode', ForUser(..))


------------------------------------------------------------------------------
-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "rd-workflows " ++ showVersion version ++ " - Copyright 2016 Hypered SPRL."


------------------------------------------------------------------------------
-- | Process the command-line choice.
processCmd :: Cmd -> IO ()
processCmd Help = print (helpText [] HelpFormatDefault workflowsModes)

processCmd Version = putStrLn versionString

processCmd None = do
  processCmd Version
  processCmd Help

processCmd CmdList{..} = (maybe call callFor cmdForUser) "reesd.dev" "workflows" "list" []

processCmd CmdAdd{..} = do
  content <- readFile cmdDefinitionPath
  (maybe call' callFor' cmdForUser)
    "reesd.dev" "workflows" "add"
    ["-"] content

processCmd CmdInstances{..} = (maybe call callFor cmdForUser)
  "reesd.dev" "workflows" "instances"
  (if cmdSentinelsOnly then ["--sentinel"] else [])

processCmd CmdInstanciate{..} = (maybe call callFor cmdForUser)
  "reesd.dev" "workflows" "instanciate" [cmdWorkflow]

processCmd CmdStep{..} = (maybe call callFor cmdForUser)
  "reesd.dev" "workflows" "step" args
  where
  args =
    ["--walk", show cmdWalkId, "--activity", cmdActivity]
    ++ maybe [] (\tag -> ["--tag", tag]) cmdTag
    -- TODO cmdResult

processCmd CmdDelete{..} = (maybe call callFor cmdForUser)
  "reesd.dev" "workflows" "delete" ["--walk", show cmdWalkId]

processCmd CmdStatus{..} = call
  "reesd.dev" "workflows" "status" []


------------------------------------------------------------------------------
-- | Data type representing the different command-line subcommands.
data Cmd =
    CmdList { cmdForUser :: Maybe String }
  | CmdAdd { cmdForUser :: Maybe String, cmdDefinitionPath :: String }
  | CmdInstances { cmdForUser :: Maybe String, cmdSentinelsOnly :: Bool }
  | CmdInstanciate { cmdForUser :: Maybe String, cmdWorkflow :: String, cmdTitle :: Maybe String }
  | CmdStep { cmdForUser :: Maybe String, cmdWalkId :: Int, cmdActivity :: String, cmdResult :: Maybe String, cmdTag :: Maybe String }
  | CmdDelete { cmdForUser :: Maybe String, cmdWalkId :: Int }
  | CmdStatus
  | Help
  | Version
  | None
  deriving Show

instance ForUser Cmd where
  setForUser r x = r { cmdForUser = Just x }

workflowsModes :: Mode Cmd
workflowsModes = (modes "workflows" None "Workflows related subcommands."
  [ workflowsListMode
  , workflowsAddMode
  , workflowsInstancesMode
  , workflowsInstanciateMode
  , workflowsStepMode
  , workflowsDeleteMode
  , workflowsStatusMode
  ])
  { modeGroupFlags = toGroup
    [ flagHelpSimple (const Help)
    , flagVersion (const Version)
    ]
  }

workflowsListMode :: Mode Cmd
workflowsListMode = mode' "list" workflowsList
  "Display available workflows."
  [forUserFlag]

workflowsAddMode :: Mode Cmd
workflowsAddMode = mode "add" workflowsAdd
  "Define a new workflow."
  (flagArg setDef "PATH")
  [forUserFlag]
  where setDef x r = Right (r { cmdDefinitionPath = x })

workflowsInstancesMode :: Mode Cmd
workflowsInstancesMode = mode' "instances" workflowsInstances
  "Display workflow instances."
  [ forUserFlag
  , flagBool ["sentinel"] (\b r -> r { cmdSentinelsOnly = b })
      "If set, display only sentinel workflows."
  ]

workflowsInstanciateMode :: Mode Cmd
workflowsInstanciateMode = mode "instanciate" workflowsInstanciate
  "Instanciate (run) a workflow."
  (flagArg setWorkflow "WORKFLOW")
  [ forUserFlag
  , flagReq ["title"] setTitle "TITLE"
      "Set the workflow title to TITLE."
  ]
  where setTitle x r = Right (r { cmdTitle = Just x })
        setWorkflow x r = Right (r { cmdWorkflow = x })

workflowsStepMode :: Mode Cmd
workflowsStepMode = mode' "step" workflowsStep
  "Step (run) a workflow instance activity."
  [ flagReq ["walk"] setWalkId "ID"
      "Workflow instance ID."
  , flagReq ["activity"] setActivity "STRING"
      "Activity to run within the workflow instance."
  , forUserFlag
  , flagReq ["tag"] setTag "TAG"
      "Set the result's tag to TAG."
  ]
  where setWalkId x r = Right (r { cmdWalkId = read x }) -- TODO Left if not an ID.
        setActivity x r = Right (r { cmdActivity = x })
        setTag x r = Right (r { cmdTag = Just x })

workflowsDeleteMode :: Mode Cmd
workflowsDeleteMode = mode' "delete" workflowsDelete
  "Delete a workflow instance."
  [ flagReq ["walk"] setWalkId "ID"
      "Workflow instance ID."
  , forUserFlag
  ]
  where setWalkId x r = Right (r { cmdWalkId = read x }) -- TODO Left if not an ID.

workflowsStatusMode :: Mode Cmd
workflowsStatusMode = mode' "status" workflowsStatus
  "Display a list of repositories being updated."
  []

workflowsList = CmdList { cmdForUser = Nothing }

workflowsAdd = CmdAdd { cmdForUser = Nothing, cmdDefinitionPath = "" }

workflowsInstances = CmdInstances { cmdForUser = Nothing, cmdSentinelsOnly = False }

workflowsInstanciate = CmdInstanciate { cmdForUser = Nothing, cmdWorkflow = "", cmdTitle = Nothing }

workflowsStep = CmdStep { cmdForUser = Nothing, cmdWalkId = 0, cmdActivity = "", cmdResult = Nothing, cmdTag = Nothing }

workflowsDelete = CmdDelete { cmdForUser = Nothing, cmdWalkId = 0 }

workflowsStatus = CmdStatus
