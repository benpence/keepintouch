module Main
( main
) where

import System.Environment(getArgs)

import KeepInTouch.Interface.CommandLine(CommandLine(dataFile),defaultCommandLine)
import KeepInTouch.Interface.CommandLine(handleArgs,handleResult)

getFile :: [String] -> (CommandLine, [String])
getFile (file : rest) = (defaultCommandLine { dataFile = file }, rest)
getFile _             = (defaultCommandLine,                     [])

main :: IO ()
main = do
    params <- getArgs
    let (interface, args) = getFile params
    result <- handleArgs interface args
    putStr $ handleResult result
