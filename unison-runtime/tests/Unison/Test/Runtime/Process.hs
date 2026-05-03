{-# LANGUAGE CPP #-}

module Unison.Test.Runtime.Process (test) where

import Control.Concurrent (threadDelay)
import EasyTest
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readCreateProcessWithExitCode, shell, waitForProcess)
import Unison.Prelude
import Unison.Runtime.Foreign.Function (startProcessWithReaper)

test :: Test ()
test =
  scope "process" do
    scope "explicit wait" do
      (_, _, _, ph) <- io $ uncurry startProcessWithReaper successfulCommand
      exitCode <- io $ waitForProcess ph
      expectEqual ExitSuccess exitCode

#ifdef linux_HOST_OS
    scope "dropped handles are reaped" do
      io $ replicateM_ 10 $ uncurry startProcessWithReaper successfulCommand
      io $ threadDelay 1_000_000
      count <- io zombieChildCount
      expectEqual 0 count
#endif

successfulCommand :: (FilePath, [String])
#ifdef mingw32_HOST_OS
successfulCommand = ("cmd", ["/c", "exit", "/b", "0"])
#else
successfulCommand = ("/bin/sh", ["-c", "exit 0"])
#endif

#ifdef linux_HOST_OS
zombieChildCount :: IO Int
zombieChildCount = do
  (exitCode, stdout, stderr) <-
    readCreateProcessWithExitCode
      (shell "ps --ppid \"$PPID\" -o stat= | awk '$1 ~ /^Z/ { count++ } END { print count + 0 }'")
      ""
  case exitCode of
    ExitSuccess ->
      case reads stdout of
        [(count, _)] -> pure count
        _ -> fail $ "Could not parse zombie child count from: " <> show stdout
    _ -> fail $ "Could not count zombie children: " <> stderr
#endif
