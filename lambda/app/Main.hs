module Main where

import           Bot.Handler.EventSubscriber (defaultHandler, runHandler)

main :: IO ()
main = runHandler defaultHandler
