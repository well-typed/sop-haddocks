{-# LANGUAGE TemplateHaskell #-}

module Generics.SOP.Haddocks.TH (
    deriveHasHaddocks
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Generics.SOP.Haddocks

-- | Derive 'HasHaddocks' instance for the given datatype
--
-- Example usage:
--
-- > -- | This is the first example
-- > data Example1 =
-- >     Constr1   -- ^ First constructor of the first example
-- >   | Constr2   -- ^ Second constructor of the first example
-- >   deriving stock (GHC.Generic)
-- >   deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
-- >
-- > deriveHasHaddocks ''Example1
--
-- NOTE: You /must/ build your code with the @ghc@ flag @-haddock@ enabled
-- <https://downloads.haskell.org/ghc/latest/docs/users_guide/using.html#ghc-flag--haddock>,
-- otherwise all docstrings will be 'Nothing'.
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

