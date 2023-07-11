{-# LANGUAGE TemplateHaskell #-}

module Generics.SOP.Haddocks.TH (
    deriveHasHaddocks
  ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Datatype
import Language.Haskell.TH

import Generics.SOP.Haddocks.Util
import Generics.SOP.Haddocks qualified as Haddocks
import Generics.SOP.Haddocks (HasHaddocks(..))

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
deriveHasHaddocks ::
     Name     -- ^ Name of the datatype or one of its constructors
  -> Q [Dec]
deriveHasHaddocks n = do
    info <- reifyDatatype n
    fmap (:[]) $
      instanceD
        (cxt [])
        (conT ''HasHaddocks `appT` conT (datatypeName info))
        [ funD 'haddocks [
              clause
                [wildP]
                (normalB (haddocksFor info))
                []
            ]
        ]

haddocksFor :: DatatypeInfo -> Q Exp
haddocksFor info = do
    typDoc <- getDoc (DeclDoc $ datatypeName info)

    if isNewtypeVariant (datatypeVariant info) then
      (conE 'Haddocks.Newtype) `appE` (lift typDoc)
    else
      (conE 'Haddocks.ADT) `appE` (lift typDoc)

