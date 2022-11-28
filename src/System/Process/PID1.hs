{-# LANGUAGE CPP #-}
module System.Process.PID1
    ( RunOptions
    , defaultRunOptions
    , getRunEnv
    , getRunExitTimeoutSec
    , getRunGroup
    , getRunUser
    , getRunWorkDir
    , run
    , runWithOptions
    , setRunEnv
    , setRunExitTimeoutSec
    , setRunGroup
    , setRunUser
    , setRunWorkDir
    ) where

import           Control.Concurrent       (forkIO, newEmptyMVar, takeMVar,
                                           threadDelay, tryPutMVar)
import           Control.Exception        (assert, catch, throwIO)
import           Control.Monad            (forever, void)
import           Data.Foldable            (for_)
import           System.Directory         (setCurrentDirectory)
import           System.Exit              (ExitCode (ExitFailure), exitWith)
import           System.IO.Error          (isDoesNotExistError)
import           System.Posix.Process     (ProcessStatus (..), executeFile,
                                           exitImmediately, getAnyProcessStatus,
                                           getProcessID)
import           System.Posix.Signals     (Handler (Catch), Signal,
                                           installHandler, sigINT, sigKILL,
                                           sigTERM, signalProcess)
import           System.Posix.Types       (CPid)
import           System.Posix.User        (getGroupEntryForName,
                                           getUserEntryForName,
                                           groupID, setGroupID,
                                           setUserID, userID)
import           System.Process           (createProcess, proc, env)
import           System.Process.Internals (ProcessHandle__ (..),
                                           modifyProcessHandle)

-- | Holder for pid1 run options
data RunOptions = RunOptions
  { -- optional environment variable override, default is current env
    runEnv :: Maybe [(String, String)]
    -- optional posix user name
  , runUser :: Maybe String
    -- optional posix group name
  , runGroup :: Maybe String
    -- optional working directory
  , runWorkDir :: Maybe FilePath
    -- timeout (in seconds) to wait for all child processes to exit after
    -- receiving SIGTERM or SIGINT signal
  , runExitTimeoutSec :: Int
  } deriving Show

-- | return default `RunOptions`
--
-- @since 0.1.1.0
defaultRunOptions :: RunOptions
defaultRunOptions = RunOptions
  { runEnv = Nothing
  , runUser = Nothing
  , runGroup = Nothing
  , runWorkDir = Nothing
  , runExitTimeoutSec = 5 }

-- | Get environment variable overrides for the given `RunOptions`
--
-- @since 0.1.1.0
getRunEnv :: RunOptions -> Maybe [(String, String)]
getRunEnv = runEnv

-- | Set environment variable overrides for the given `RunOptions`
--
-- @since 0.1.1.0
setRunEnv :: [(String, String)] -> RunOptions -> RunOptions
setRunEnv env' opts = opts { runEnv = Just env' }

-- | Get the process 'setUserID' user for the given `RunOptions`
--
-- @since 0.1.1.0
getRunUser :: RunOptions -> Maybe String
getRunUser = runUser

-- | Set the process 'setUserID' user for the given `RunOptions`
--
-- @since 0.1.1.0
setRunUser :: String -> RunOptions -> RunOptions
setRunUser user opts = opts { runUser = Just user }

-- | Get the process 'setGroupID' group for the given `RunOptions`
--
-- @since 0.1.1.0
getRunGroup :: RunOptions -> Maybe String
getRunGroup = runGroup

-- | Set the process 'setGroupID' group for the given `RunOptions`
--
-- @since 0.1.1.0
setRunGroup :: String -> RunOptions -> RunOptions
setRunGroup group opts = opts { runGroup = Just group }

-- | Get the process current directory for the given `RunOptions`
--
-- @since 0.1.1.0
getRunWorkDir :: RunOptions -> Maybe FilePath
getRunWorkDir = runWorkDir

-- | Set the process current directory for the given `RunOptions`
--
-- @since 0.1.1.0
setRunWorkDir :: FilePath -> RunOptions -> RunOptions
setRunWorkDir dir opts = opts { runWorkDir = Just dir }

-- | Return the timeout (in seconds) timeout (in seconds) to wait for all child
-- processes to exit after receiving SIGTERM or SIGINT signal
--
-- @since 0.1.2.0
getRunExitTimeoutSec :: RunOptions -> Int
getRunExitTimeoutSec = runExitTimeoutSec

-- | Set the timeout in seconds for the process reaper to wait for all child
-- processes to exit after receiving SIGTERM or SIGINT signal
--
-- @since 0.1.2.0
setRunExitTimeoutSec :: Int -> RunOptions -> RunOptions
setRunExitTimeoutSec sec opts = opts { runExitTimeoutSec = sec }

-- | Run the given command with specified arguments, with optional environment
-- variable override (default is to use the current process's environment).
--
-- This function will check if the current process has a process ID of 1. If it
-- does, it will install signal handlers for SIGTERM and SIGINT, set up a loop
-- to reap all orphans, spawn a child process, and when that child dies, kill
-- all other processes (first with a SIGTERM and then a SIGKILL) and exit with
-- the child's exit code.
--
-- If this process is not PID1, then it will simply @exec@ the given command.
--
-- This function will never exit: it will always terminate your process, unless
-- some exception is thrown.
--
-- @since 0.1.0.0
run :: FilePath -- ^ command to run
    -> [String] -- ^ command line arguments
    -> Maybe [(String, String)]
    -- ^ optional environment variable override, default is current env
    -> IO a
run cmd args env' = runWithOptions (defaultRunOptions {runEnv = env'}) cmd args

-- | Variant of 'run' that runs a command, with optional environment posix
-- user/group and working directory (default is to use the current process's
-- user, group, environment, and current directory).
--
-- @since 0.1.1.0
runWithOptions :: RunOptions -- ^ run options
               -> FilePath -- ^ command to run
               -> [String] -- ^ command line arguments
               -> IO a
runWithOptions opts cmd args = do
  for_ (runGroup opts) $ \name -> do
    entry <- getGroupEntryForName name
    setGroupID $ groupID entry
  for_ (runUser opts) $ \name -> do
    entry <- getUserEntryForName name
    setUserID $ userID entry
  for_ (runWorkDir opts) setCurrentDirectory
  let env' = runEnv opts
      timeout = runExitTimeoutSec opts
  -- check if we should act as pid1 or just exec the process
  myID <- getProcessID
  if myID == 1
    then runAsPID1 cmd args env' timeout
    else executeFile cmd True args env'

-- | Run as a child with signal handling and orphan reaping.
runAsPID1 :: FilePath -> [String] -> Maybe [(String, String)] -> Int -> IO a
runAsPID1 cmd args env' timeout = do
    -- Set up an MVar to indicate we're ready to start killing all
    -- children processes. Then start a thread waiting for that
    -- variable to be filled and do the actual killing.
    killChildrenVar <- newEmptyMVar

    -- Helper function to start killing, used below
    let startKilling = void $ tryPutMVar killChildrenVar ()

    -- Install signal handlers for TERM and INT, which will start
    -- killing all children
    void $ installHandler sigTERM (Catch startKilling) Nothing
    void $ installHandler sigINT  (Catch startKilling) Nothing

    -- Spawn the child process
    (Nothing, Nothing, Nothing, ph) <- createProcess (proc cmd args)
        { env = env'
        }

    -- Determine the child PID. We want to exit once this child
    -- process is dead.
    p_ <- modifyProcessHandle ph $ \p_ -> return (p_, p_)
    child <-
        case p_ of
            ClosedHandle e -> assert False (exitWith e)
            OpenHandle pid -> return pid
            OpenExtHandle pid _ -> return pid

    _ <- forkIO $ do
        takeMVar killChildrenVar
        killAllChildren child timeout

    -- Loop on reaping child processes
    reap startKilling child

reap :: IO () -> CPid -> IO a
reap startKilling child = do
    -- Track the ProcessStatus of the child
    childStatus <- newEmptyMVar

    -- Keep reaping one child. Eventually, when all children are dead,
    -- we'll get an exception. We catch that exception and, assuming
    -- it's the DoesNotExistError we're expecting, know that all
    -- children are dead and exit.
    forever (reapOne childStatus) `catch` \e ->
        if isDoesNotExistError e
            -- no more child processes
            then do
                takeMVar childStatus >>= exitImmediately . toExitCode
                error "This can never be reached"
            -- some other exception occurred, reraise it
            else throwIO e
  where
    reapOne childStatus = do
        -- Block until a child process exits
        mres <- getAnyProcessStatus True False
        case mres of
            -- This should never happen, if there are no more child
            -- processes we'll get an exception instead
            Nothing -> assert False (return ())
            -- Got a new dead child. If it's the child we created in
            -- main, then start killing all other children. Otherwise,
            -- we're just reaping.
            Just (pid, status)
                | pid == child -> do
                    -- Take the first status of the child. It's possible -
                    -- however unlikely - that the process ID could end up
                    -- getting reused and there will be another child exiting
                    -- with the same PID. Just ignore that.
                    void $ tryPutMVar childStatus status
                    startKilling
                | otherwise -> return ()

killAllChildren :: CPid -> Int -> IO ()
killAllChildren cid timeout = do
    -- Send the direct child process the TERM signal
    signalProcess sigTERM cid `catch` \e ->
        if isDoesNotExistError e
            then return ()
            else throwIO e

    -- Wait for `timeout` seconds and allow the 'main' process to take care
    -- of shutting down any child processes itself.
    threadDelay $ timeout * 1000 * 1000

    -- If the 'main' process did not handle shutting down the rest of the
    -- child processes we will signal SIGTERM to them directly.
    signalProcess sigTERM (-1) `catch` \e ->
        if isDoesNotExistError e
            then return ()
            else throwIO e

    -- Wait for `timeout` seconds. We don't need to put in any logic about
    -- whether there are still child processes; if all children have
    -- exited, then the reap loop will exit and our process will shut
    -- down.
    threadDelay $ timeout * 1000 * 1000

    -- OK, some children didn't exit. Now time to get serious!
    signalProcess sigKILL (-1) `catch` \e ->
        if isDoesNotExistError e
            then return ()
            else throwIO e

-- | Convert a ProcessStatus to an ExitCode. In the case of a signal being the
-- cause of termination, see 'signalToEC'.
toExitCode :: ProcessStatus -> ExitCode
toExitCode (Exited ec) = ec
#if MIN_VERSION_unix(2, 7, 0)
toExitCode (Terminated sig _) = signalToEC sig
#else
toExitCode (Terminated sig) = signalToEC sig
#endif
toExitCode (Stopped sig) = signalToEC sig

-- | Follow the convention of converting a signal into an exit code by adding
-- 128.
signalToEC :: Signal -> ExitCode
signalToEC sig = ExitFailure (fromIntegral sig + 128)
