{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString.Lazy     (ByteString)
import           Network.HTTP.Types       (status202)
import           Network.Wai              (Application, Request (..),
                                           responseLBS)
import           Network.Wai.Handler.Warp (run)


main :: IO ()
main = run 8000 server

server :: Application
server req respond =
  case pathInfo req of
    ["main.js"] ->
      respond $ responseLBS
      status202
      [("Content-Type", "text/javascript")]
      mainJs
    _ ->
      respond $ responseLBS
      status202
      [("Content-Type", "text/html")]
      indexHtml

mainJs :: ByteString
mainJs = "main.js"

indexHtml :: ByteString
indexHtml = "index.html"
