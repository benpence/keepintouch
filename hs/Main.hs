module Main
( main
) where

import System.Environment(getArgs)

import KeepInTouch.Interface.CommandLine(handleInput)

main :: IO ()
main = getArgs >>= handleInput >>= putStr
