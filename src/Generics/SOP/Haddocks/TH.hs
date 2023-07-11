{-# LANGUAGE TemplateHaskell #-}

module Generics.SOP.Haddocks.TH (
    deriveHasHaddocks
  ) where

import Data.SOP
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Datatype
import Language.Haskell.TH

import Generics.SOP.Haddocks qualified as Haddocks
import Generics.SOP.Haddocks (HasHaddocks(..), Haddocks(..))

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

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
        [ funD 'getHaddocks [
              clause
                [wildP]
                (normalB (mkHaddocks info))
                []
            ]
        ]

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

-- | Construct 'Haddocks'
mkHaddocks :: DatatypeInfo -> Q Exp
mkHaddocks info = do
    typDoc <- getDoc (DeclDoc $ datatypeName info)
    conE 'Haddocks
      `appE` lift typDoc
      `appE` mkNP (map mkConstructorInfo $ datatypeCons info)

-- | Construct 'Haddocks.ConstructorInfo'
mkConstructorInfo :: ConstructorInfo -> Q Exp
mkConstructorInfo info = do
    conDoc <- getDoc (DeclDoc $ constructorName info)
    case isRecordVariant (constructorVariant info) of
      Just names ->
        conE 'Haddocks.Constructor
          `appE` lift conDoc
          `appE` mkNP (map mkFieldInfo names)
      Nothing ->
        conE 'Haddocks.Constructor
          `appE` lift conDoc
          -- TODO: This is wrong. Constructor arguments /can/ have docs
          `appE` mkNP (map (const noFieldInfo) (constructorFields info))

mkFieldInfo :: Name -> Q Exp
mkFieldInfo field = do
    fieldDoc <- getDoc (DeclDoc field)
    conE 'Haddocks.FieldInfo
      `appE` lift fieldDoc

noFieldInfo :: Q Exp
noFieldInfo = conE 'Haddocks.FieldInfo `appE` conE 'Nothing

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

mkNP :: [Q Exp] -> Q Exp
mkNP []       = conE 'Nil
mkNP (e : es) = infixE (Just e) (conE '(:*)) (Just $ mkNP es)

-- | Does this variant correspond to a record?
isRecordVariant :: ConstructorVariant -> Maybe [Name]
isRecordVariant NormalConstructor         = Nothing
isRecordVariant InfixConstructor          = Nothing
isRecordVariant (RecordConstructor names) = Just names
