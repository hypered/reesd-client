module Reesd.Client.Helpers where

import System.Console.CmdArgs.Explicit
  ( flagReq, modeArgs
  , modeEmpty, modeGroupFlags, modeHelp, toGroup
  , Flag, Help, Mode(..), Name
  )
import System.Process (createProcess, proc, readProcess, waitForProcess)

-- TODO Document the following bits:
-- Using admin.reesd.com instead of reesd.com is useful for .ssh/config with
-- IdentityFile + IdentitiesOnly, but has otherwise no other purpose.
-- admin.reesd.com should be a CNAME for reesd.com.


------------------------------------------------------------------------------
-- | Call `reesd-command` through SSH.
call :: String -> String -> [String] -> IO ()
call command subcommand args = do
  (_, _, _, h) <- createProcess
    (proc "ssh" (["rd@reesd.dev", "reesd-command", command, subcommand] ++ args))
  _ <- waitForProcess h
  return ()

callFor :: String -> String -> String -> [String] -> IO ()
callFor user command subcommand args = do
  (_, _, _, h) <- createProcess
    (proc "ssh" (["rdadmin@admin.reesd.dev", "reesd-admin", "--for", user, command, subcommand] ++ args))
  _ <- waitForProcess h
  return ()

call' :: String -> String -> [String] -> String -> IO ()
call' command subcommand args input = do
  out <- readProcess
    "ssh" (["rd@reesd.dev", "reesd-command", command, subcommand] ++ args)
    input
  putStr out
  return ()

callFor' :: String -> String -> String -> [String] -> String -> IO ()
callFor' user command subcommand args input = do
  out <- readProcess
    "ssh" (["rdadmin@admin.reesd.dev", "reesd-admin", "--for", user, command, subcommand] ++ args)
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
