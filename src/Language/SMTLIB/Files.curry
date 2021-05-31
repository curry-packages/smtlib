--- ----------------------------------------------------------------------------
--- This module provides some file operations for SMT-LIB.
---
--- @author  Jan Tikovsky
--- @version May 2021
--- ----------------------------------------------------------------------------
module Language.SMTLIB.Files (writeSMT, writeSMTDump) where

import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath  ((</>), (<.>), takeBaseName)
import Data.Time        (getLocalTime, calendarTimeToString)

import Language.SMTLIB.Pretty (showSMT)
import Language.SMTLIB.Types

--- Dump an SMT-LIB script to the hidden folder .smt in the current directory
writeSMTDump :: String -> [Command] -> IO ()
writeSMTDump fn cmds = do
  cdir <- getCurrentDirectory
  let dumpDir = cdir </> ".smt"
  createDirectoryIfMissing True dumpDir
  dumpFilePath <- getUniqueFP dumpDir (takeBaseName fn) "smt2"
  writeSMT dumpFilePath cmds

--- Write an SMT-LIB script to a file
writeSMT :: String -> [Command] -> IO ()
writeSMT fn cmds = writeFile fn (showSMT cmds)

-- helper
-- Generate a unique file path with the given path, prefix and file extension
getUniqueFP :: String -> String -> String -> IO String
getUniqueFP path prefix ext = do
  tstmp <- getLocalTime
  return $ path </> prefix ++ calendarTimeToString tstmp <.> ext
