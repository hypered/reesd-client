{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | cmdargs definitions for the `rd-workflows` executable.
module Reesd.Client.Commands.Workflows where

import Data.Version (showVersion)
import Paths_reesd_client (version)
import System.Console.CmdArgs.Explicit
  ( flagArg, flagHelpSimple, flagReq, flagVersion, helpText, mode, modeArgs
  , modeEmpty, modeGroupFlags, modeHelp, modes, toGroup
  , Flag, Help, HelpFormat(..), Mode(..), Name
  )
import System.Process (createProcess, proc, waitForProcess)


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

processCmd CmdList{..} = call "list"

processCmd CmdInstances{..} = call "instances"

processCmd CmdInstanciate{..} = call "instanciate"

processCmd CmdStep{..} = call "step"

processCmd CmdStatus{..} = call "status"


------------------------------------------------------------------------------
-- | Data type representing the different command-line subcommands.
data Cmd =
    CmdList
  | CmdInstances
  | CmdInstanciate { cmdWorkflow :: String }
  | CmdStep { cmdWalkId :: Int, cmdActivity :: String }
  | CmdStatus
  | Help
  | Version
  | None
  deriving Show

workflowsModes :: Mode Cmd
workflowsModes = (modes "workflows" None "Workflows related subcommands."
  [ workflowsListMode
  , workflowsInstancesMode
  , workflowsInstanciateMode
  , workflowsStepMode
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
  []

workflowsInstancesMode :: Mode Cmd
workflowsInstancesMode = mode' "instances" workflowsInstances
  "Display workflow instances."
  []

workflowsInstanciateMode :: Mode Cmd
workflowsInstanciateMode = mode "instanciate" workflowsInstanciate
  "Instanciate (run) a workflow."
  (flagArg setWorkflow "WORKFLOW")
  []
  where setWorkflow x r = Right (r { cmdWorkflow = x })

workflowsStepMode :: Mode Cmd
workflowsStepMode = mode' "step" workflowsStep
  "Step (run) a workflow instance activity."
  [ flagReq ["walk"] setWalkId "ID"
      "Workflow instance ID."
  , flagReq ["activity"] setActivity "STRING"
      "Activity to run within the workflow instance."
  ]
  where setWalkId x r = Right (r { cmdWalkId = read x }) -- TODO Left if not an ID.
        setActivity x r = Right (r { cmdActivity = x })

workflowsStatusMode :: Mode Cmd
workflowsStatusMode = mode' "status" workflowsStatus
  "Display a list of repositories being updated."
  []

workflowsList = CmdList

workflowsInstances = CmdInstances

workflowsInstanciate = CmdInstanciate ""

workflowsStep = CmdStep 0 ""

workflowsStatus = CmdStatus


------------------------------------------------------------------------------
-- | Call `reesd-command` through SSH.
call command = do
  (_, _, _, h) <- createProcess
    (proc "ssh" ["rd@reesd.dev", "reesd-command", "workflows", command])
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
