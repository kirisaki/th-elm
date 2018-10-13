{-# LANGUAGE TemplateHaskell #-}
module TH where

import           Language.Haskell.TH (Exp, Q, runIO)

loadFile :: FilePath -> Q Exp
loadFile path = do
  str <- runIO $ readFile path
  [| str |]
