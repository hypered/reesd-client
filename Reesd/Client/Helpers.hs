module Reesd.Client.Helpers where

import System.Console.CmdArgs.Explicit
  ( flagReq, modeArgs
  , modeEmpty, modeGroupFlags, modeHelp, toGroup
  , Flag, Help, Mode(..), Name
  )
import System.Process (createProcess, proc, readProcess, waitForProcess)


------------------------------------------------------------------------------
-- | Call `reesd-command` through SSH.
-- Normal user commands.
call :: String -> String -> String -> [String] -> IO ()
call domain command subcommand args = do
  (_, _, _, h) <- createProcess
    (proc "ssh" (["rd@" ++ domain, "reesd-command", command, subcommand] ++ args))
  _ <- waitForProcess h
  return ()

-- | Normal user commands, but called by the admin on their behalf.
callFor :: String -> String -> String -> String -> [String] -> IO ()
callFor user domain command subcommand args = do
  (_, _, _, h) <- createProcess
    (proc "ssh" (["rdadmin@" ++ domain, "reesd-admin", "--for", user, command, subcommand] ++ args))
  _ <- waitForProcess h
  return ()

-- | Admin commands.
callAdmin :: String -> String -> String -> [String] -> IO ()
callAdmin domain command subcommand args = do
  (_, _, _, h) <- createProcess
    (proc "ssh" (["rdadmin@" ++ domain, "reesd-admin", command, subcommand] ++ args))
  _ <- waitForProcess h
  return ()

-- Normal user commands.
call' :: String -> String -> String -> [String] -> String -> IO ()
call' domain command subcommand args input = do
  out <- readProcess
    "ssh" (["rd@" ++ domain, "reesd-command", command, subcommand] ++ args)
    input
  putStr out
  return ()

-- | Normal user commands, but called by the admin on their behalf.
callFor' :: String -> String -> String -> String -> [String] -> String -> IO ()
callFor' user domain command subcommand args input = do
  out <- readProcess
    "ssh" (["rdadmin@" ++ domain, "reesd-admin", "--for", user, command, subcommand] ++ args)
    input
  putStr out
  return ()

-- | Admin commands.
callAdmin' :: String -> String -> String -> [String] -> String -> IO ()
callAdmin' domain command subcommand args input = do
  out <- readProcess
    "ssh" (["rdadmin@" ++ domain, "reesd-admin", command, subcommand] ++ args)
    input
  putStr out
  return ()


------------------------------------------------------------------------------
class ForUser r where
  setForUser :: r -> String -> r

forUserFlag :: ForUser r => Flag r
forUserFlag =
  flagReq ["for"]
    (\x r -> Right (setForUser r x))
    "USERNAME"
    "Username for which the command should be done."

------------------------------------------------------------------------------
-- | Same as `mode` but without an `Arg a` argument.
mode' :: Name -> a -> Help -> [Flag a] -> Mode a
mode' name value help flags = (modeEmpty value)
  { modeNames = [name]
  , modeHelp = help
  , modeArgs = ([], Nothing)
  , modeGroupFlags = toGroup flags
  }
