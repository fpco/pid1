module Main (main) where
-- | This is a valid PID 1 process in Haskell, intended as a Docker
-- entrypoint. It will handle reaping orphans and handling TERM and
-- INT signals.

import Data.Maybe (fromMaybe)
import System.Process.PID1
import System.Environment
import System.Console.GetOpt
import System.IO (stderr, hPutStr)
import System.Exit (exitFailure)

-- | `GetOpt` command line options
options :: [(String, String)] -> [OptDescr (RunOptions -> RunOptions)]
options defaultEnv =
  [ Option ['e'] ["env"] (ReqArg (\opt opts -> setRunEnv (optEnvList (getRunEnv opts) opt) opts) "ENV") "override environment variable from given name=value pair. Can be specified multiple times to set multiple environment variables"
  , Option ['u'] ["user"] (ReqArg setRunUser "USER") "run command as user"
  , Option ['g'] ["group"] (ReqArg setRunGroup "GROUP") "run command as group"
  , Option ['w'] ["workdir"] (ReqArg setRunWorkDir "DIR") "command working directory"
  , Option ['t'] ["timeout"] (ReqArg (setRunExitTimeoutSec . read) "TIMEOUT") "timeout (in seconds) to wait for all child processes to exit" ]
  where optEnv env' kv =
          let kvp = fmap (drop 1) $ span (/= '=') kv in
            kvp:filter ((fst kvp /=) . fst) env'
        optEnvList = optEnv . fromMaybe defaultEnv

main :: IO ()
main = do
    -- Figure out the actual thing to run and spawn it off.
    args0 <- getArgs
    defaultEnv <- getEnvironment
    progName <- getProgName
    let opts = options defaultEnv
    case getOpt RequireOrder opts args0 of
      (o, (cmd:args), []) -> let runOpts = foldl (flip id) defaultRunOptions o in
        runWithOptions runOpts cmd args
      _ -> do
        let usage = "Usage: " ++ progName ++ " [OPTION...] command [args...]"
        hPutStr stderr (usageInfo usage opts)
        exitFailure
