module Main where

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
