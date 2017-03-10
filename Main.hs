module Main where

import Check
import Api

import Network.Wai.Handler.Warp


main :: IO ()
main = Api.server >>= run 8080
