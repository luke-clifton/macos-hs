{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import System.Exit
import System.Environment
import MacOS.Keychain
import Data.ByteString.Char8 as BS
import System.IO

main :: IO ()
main = do
    getArgs >>= \case
        [s,u,p] -> setPassword (Service $ pack s) (User $ pack u) (Password $ pack p)
        [s,u] -> printPass s u
        [s]   -> printPass s ""
        _     -> usage

printPass :: String -> String -> IO ()
printPass s u = do
    Password p <- getPassword (Service $ pack s) (User $ pack u)
    BS.putStr p

usage :: IO ()
usage = do
    System.IO.hPutStrLn stderr "Usage: getpass <service> [<user>] [<new-password>]"
    exitFailure
