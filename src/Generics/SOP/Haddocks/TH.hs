{-# LANGUAGE TemplateHaskell #-}

module Generics.SOP.Haddocks.TH (
    deriveHasHaddocks
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Generics.SOP.Haddocks

deriveHasHaddocks :: Name -> Q [Dec]
deriveHasHaddocks typ = fmap (:[]) $
    instanceD
      (cxt [])
      (conT ''HasHaddocks `appT` conT typ)
      [ funD 'haddocks [
            clause
              [wildP]
              (normalB (haddocksFor typ))
              []
          ]
      ]

haddocksFor :: Name -> Q Exp
haddocksFor typ = do
    typDoc <- getDoc (DeclDoc typ)
    (conE 'ADT) `appE` (lift typDoc)

