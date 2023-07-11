{-# LANGUAGE TemplateHaskell #-}

module Generics.SOP.Haddocks.TH (
    deriveHasHaddocks
  ) where

import Language.Haskell.TH

import Generics.SOP.Haddocks

deriveHasHaddocks :: Name -> Q [Dec]
deriveHasHaddocks typ = do
    inst <-
      instanceD
        (cxt [])
        (conT ''HasHaddocks `appT` conT typ)
        []

    return [inst]
