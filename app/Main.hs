{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import Posts

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs [url] = listUrlsForSite url
handleArgs _     = putStrLn "usage: list-all-images URL"
