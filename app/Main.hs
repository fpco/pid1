module Main where

-- | This is a valid PID 1 process in Haskell, intended as a Docker
-- entrypoint. It will handle reaping orphans and handling TERM and
-- INT signals.
import System.Process.PID1
import System.Environment

main :: IO ()
main = do
    -- Figure out the actual thing to run and spawn it off.
    args0 <- getArgs

    (cmd, args) <-
        case args0 of
            [] -> error "No arguments provided"
            cmd:args -> return (cmd, args)

    run cmd args Nothing
