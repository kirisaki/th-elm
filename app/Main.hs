{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           TH                       (loadFile)

import           Data.ByteString.Lazy     (ByteString)
import           Data.Text.Lazy           (pack)
import           Data.Text.Lazy.Encoding  (encodeUtf8)
import           Network.HTTP.Types       (status202)
import           Network.Wai              (Application, Request (..),
                                           responseLBS)
import           Network.Wai.Handler.Warp (run)


main :: IO ()
main = do
  putStrLn "Running at \"http://localhost:8000\""
  run 8000 server

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
mainJs = (encodeUtf8 . pack) $(loadFile "client/dist/main.js")

indexHtml :: ByteString
indexHtml = (encodeUtf8 . pack) $(loadFile "client/dist/index.html")
