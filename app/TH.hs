{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module TH where

import           Control.Lens               ((^.))
import           Control.Monad              (sequence)
import           Data.Aeson                 (Result (..), Value, fromJSON)
import           Data.Aeson.Lens            (key, _Array)
import           Data.Text                  (Text, isPrefixOf, unpack)
import           Data.Vector                (toList)
import           Data.Yaml                  (decodeFileThrow)
import           Language.Haskell.TH.Syntax (Exp, Q, addDependentFile, runIO)

loadFile :: FilePath -> Q Exp
loadFile path = do
  str <- runIO $ readFile path
  [| str |]

build :: Q Exp
build = do
  y <- decodeFileThrow "package.yaml" :: Q Value
  let deps =
        map unpack .
        filter (isPrefixOf "client") .
        (\(Success t) -> t) .
        mapM fromJSON .
        toList $ y ^. key "extra-source-files" ._Array
  mapM_ addDependentFile deps
  [| return () |]
