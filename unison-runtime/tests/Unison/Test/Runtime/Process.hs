{-# LANGUAGE CPP #-}

module Unison.Test.Runtime.Process (test) where

#ifdef linux_HOST_OS
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
#endif
import EasyTest
import System.Exit (ExitCode (ExitSuccess))
import System.IO (Handle)
import System.Process (ProcessHandle, waitForProcess)
#ifdef linux_HOST_OS
import System.Process (readCreateProcessWithExitCode, shell)
#endif
import Unison.Runtime.Foreign.Function (ForeignConvention (decodeVal, encodeVal), foreignCall)
import Unison.Runtime.Foreign.Function.Type (ForeignFunc (IO_process_start))
import Unison.Runtime.MCode (Args (VArg2))
import Unison.Runtime.Stack (alloc, bumpn, exStackIOToIO, peek, pokeOff, unpackXStack)
import Unison.Util.Text qualified as Util.Text

test :: Test ()
test =
  scope "process" do
    scope "explicit wait" do
      (_, _, _, ph) <- io $ startInteractiveProcessViaForeignCall successfulCommand
      exitCode <- io $ waitForProcess ph
      expectEqual ExitSuccess exitCode

#ifdef linux_HOST_OS
    scope "dropped handles are reaped" do
      io $ replicateM_ 10 $ startInteractiveProcessViaForeignCall successfulCommand
      io $ threadDelay 1000000
      count <- io zombieChildCount
      expectEqual 0 count
#endif

startInteractiveProcessViaForeignCall :: (FilePath, [String]) -> IO (Handle, Handle, Handle, ProcessHandle)
startInteractiveProcessViaForeignCall (exe, args) = do
  stk0 <- alloc >>= \stk -> bumpn stk 2
  pokeOff stk0 1 (encodeVal exe)
  pokeOff stk0 0 (encodeVal (Util.Text.pack <$> args))
  (_, stk1) <- exStackIOToIO $ foreignCall IO_process_start (VArg2 1 0) (unpackXStack stk0)
  decodeVal =<< peek stk1

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
