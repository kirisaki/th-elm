{-# LANGUAGE TemplateHaskell #-}
module TH where

import           Control.Monad              (sequence)
import           Language.Haskell.TH.Syntax (Exp, Q, addDependentFile, runIO)
import           System.Directory           (getCurrentDirectory, listDirectory)

loadFile :: FilePath -> Q Exp
loadFile path = do
  list <- runIO $ listDirectory =<< getCurrentDirectory
  mapM_ addDependentFile list
  runIO $ appendFile "hoge.txt" (path ++ "\n")
  str <- runIO $ readFile path
  [| str |]

build :: Q Exp
build = do
  runIO $ appendFile "hoge.txt" "<build>\n"
  [| return () |]
