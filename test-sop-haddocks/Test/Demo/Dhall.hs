{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Demo.Dhall (tests) where

import Control.Monad
import Control.Monad.State
import Data.Function (on)
import Data.List (intercalate, nubBy)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
import Dhall qualified
import Generics.SOP
import Generics.SOP qualified as SOP
import GHC.Generics qualified as GHC
import GHC.Natural
import Test.Tasty
import Test.Tasty.HUnit

import Generics.SOP.Haddocks (HasHaddocks(..), Haddocks(..))
import Generics.SOP.Haddocks qualified as Haddocks
import Generics.SOP.Haddocks.TH

{-------------------------------------------------------------------------------
  Proof-of-concept Haskell-to-Dhall translation

  This is a demo only; there are missing cases, and almost certainly also
  incorrect cases. Also, the way we've set things up here will loop when there
  are mutually recursive datatypes.
-------------------------------------------------------------------------------}

type GenType = State [(DatatypeName, String)]

class ToDhall a where
  -- | Compute Dhall type corresponding to the Haskell type
  --
  -- May add additional bindings into the environment from 'GenType'. E.g.,
  --
  -- > newtype Day = MkDay Natural  -- ^ Between 1 and 7
  -- >
  -- > data SpecificDay = MkSpecificDay {
  -- >       year    :: Natural  -- ^ After 1970
  -- >     , month   :: Natural  -- ^ Between 1 and 12
  -- >     , day     :: Natural  -- ^ Between 1 and 31
  -- >     , weekday :: Day      -- ^ Day of the week
  -- >     }
  --
  -- will result in the below (layout slightly cleaned up):
  --
  -- > let Day         = Natural -- Between 1 and 7
  -- > let SpecificDay = {
  -- >         year    : Natural -- After 1970
  -- >       , month   : Natural -- Between 1 and 12
  -- >       , day     : Natural -- Between 1 and 31
  -- >       , weekday : Day     -- Day of the week
  -- >       }
  --
  -- Note that all comments are preserved (the main point of this demo).
  toDhallType :: proxy a -> GenType String

  -- | Construct Dhall expression from a Haskell value
  --
  -- To continue the same example, the Haskell value
  --
  -- > MkSpecificDay 2023 07 11 (MkDay 1)
  --
  -- will result in (layout again slightly cleaned up):
  --
  -- > { year    = 2023
  -- > , month   = 7
  -- > , day     = 11
  -- > , weekday = 1
  -- > }
  toDhallExpr :: a -> String

  default toDhallType ::
       ( HasDatatypeInfo a
       , HasHaddocks a
       , All2 ToDhall (Code a)
       )
    => proxy a -> GenType String
  toDhallType = gtoDhallType

  default toDhallExpr ::
       ( HasDatatypeInfo a
       , All2 ToDhall (Code a)
       )
    => a -> String
  toDhallExpr = gtoDhallExpr

instance ToDhall Natural where
  toDhallType _ = return "Natural"
  toDhallExpr = show

instance ToDhall Bool where
  toDhallType _ = return "Bool"
  toDhallExpr = show

instance ToDhall Text where
  toDhallType _ = return "Text"
  toDhallExpr = show

{-------------------------------------------------------------------------------
  Generic construction of the Dhall type
-------------------------------------------------------------------------------}

gtoDhallType :: forall proxy a.
     ( HasDatatypeInfo a
     , HasHaddocks a
     , All2 ToDhall (Code a)
     )
  => proxy a -> GenType String
gtoDhallType p = do
    typDef <- case constructorInfo (datatypeInfo p) of
      constrInfo :* Nil ->
        dhallTypeForConstr constrInfo (hd constrDocs)
      constrs -> do
        typs <- sequence . hcollapse $
                  hczipWith
                    (Proxy @(All ToDhall))
                    dhallTypeForUnion
                    constrs
                    constrDocs
        return $ "< " <> intercalate "\n| " typs <> "\n>"
    modify $ (:) (typName, typDef)
    return typName
  where
    typName = datatypeName (datatypeInfo p)
    Haddocks _ constrDocs = getHaddocks p

dhallTypeForConstr :: forall xs.
     All ToDhall xs
  => ConstructorInfo xs
  -> Haddocks.Constructor xs
  -> GenType String
dhallTypeForConstr constrInfo constrDocs =
    case (constrInfo, fieldDocs) of
      (Constructor _, _ :* Nil) ->
        singleArg
      (Record _ fields, _) -> do
        typs <- sequence . hcollapse $
                  hczipWith
                    (Proxy @ToDhall)
                    recordField
                    fields
                    fieldDocs
        return $ "{ " <> intercalate "\n, " typs <> "\n}"
      _otherwise ->
        error "unsupported Dhall type"
  where
    constrDoc :: Haddocks.Doc
    fieldDocs :: NP Haddocks.Argument xs
    Haddocks.Constructor constrDoc fieldDocs = constrDocs

    singleArg :: forall x. xs ~ '[x] => GenType String
    singleArg = do
        typ <- toDhallType (Proxy @x)
        return $ typ <> " -- " <> fromMaybe "" constrDoc

    recordField :: forall x.
         ToDhall x
      => FieldInfo x -> Haddocks.Argument x -> K (GenType String) x
    recordField (FieldInfo name) (Haddocks.Argument argDoc) = K $ do
        typ <- toDhallType (Proxy @x)
        return $ name <> " : " <> typ <> " -- " <> fromMaybe "" argDoc

dhallTypeForUnion :: forall xs.
     All ToDhall xs
  => ConstructorInfo xs
  -> Haddocks.Constructor xs
  -> K (GenType String) xs
dhallTypeForUnion constrInfo constrDocs = K $ do
    typ <- dhallTypeForConstr constrInfo constrDocs
    return $ constructorName constrInfo <> " : " <> typ

{-------------------------------------------------------------------------------
  Generic construction of Dhall expression
-------------------------------------------------------------------------------}

gtoDhallExpr :: forall a.
     ( HasDatatypeInfo a
     , All2 ToDhall (Code a)
     )
  => a -> String
gtoDhallExpr a =
    case (datatypeInfo (Proxy @a), from a) of
      (Newtype _ _ constrInfo, SOP x) ->
        dhallExprForConstr constrInfo (unZ x)
      (ADT _ _ (constrInfo :* Nil) _, SOP x) ->
        dhallExprForConstr constrInfo (unZ x)
      (ADT _ typName constrs _, SOP x) ->
        hcollapse $
          hczipWith
            (Proxy @(All ToDhall))
            (dhallExprForUnion typName)
            constrs
            x

dhallExprForConstr ::
     All ToDhall xs
  => ConstructorInfo xs -> NP I xs -> String
dhallExprForConstr constrInfo value =
    case (constrInfo, value) of
      (Constructor _, I arg :* Nil) ->
        -- Regular constructor, single argument
        toDhallExpr arg
      (Record _ names, _) -> mconcat [
          "{ "
        , intercalate "\n, " . hcollapse $
            hczipWith (Proxy @ToDhall) recordField names value
        , "\n}"
        ]
      _otherwise ->
        error "unsupported Dhall expr"
  where
    recordField :: ToDhall x => FieldInfo x -> I x -> K String x
    recordField (FieldInfo name) (I x) = K $ name <> " = " <> toDhallExpr x

dhallExprForUnion ::
     All ToDhall xs
  => DatatypeName -> ConstructorInfo xs -> NP I xs -> K String xs
dhallExprForUnion typName constr xs = K $ mconcat [
      typName <> "." <> constructorName constr
    , " "
    , dhallExprForConstr constr xs
    ]

{-------------------------------------------------------------------------------
  Examples

  This is a proof of concept only; for simplicity, we do not support unions
  with no-argument constructors.
-------------------------------------------------------------------------------}

newtype Day = MkDay Natural  -- ^ Between 1 and 7
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

data SpecificDay = MkSpecificDay {
      year    :: Natural  -- ^ After 1970
    , month   :: Natural  -- ^ Between 1 and 12
    , day     :: Natural  -- ^ Between 1 and 31
    , weekday :: Day      -- ^ Day of the week
    }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

data Appointment =
    Weekly Day         -- ^ Every week, on the given day
  | Once SpecificDay   -- ^ Once, on the specified date
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

deriveHasHaddocks ''Day
deriveHasHaddocks ''SpecificDay
deriveHasHaddocks ''Appointment

instance ToDhall Day
instance ToDhall SpecificDay
instance ToDhall Appointment

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Demo.Dhall" [
      testCaseInfo "Natural"     $ test $ (1 :: Natural)
    , testCaseInfo "Bool"        $ test $ True
    , testCaseInfo "Text"        $ test $ ("hi" :: Text)
    , testCaseInfo "Day"         $ test $ MkDay 1
    , testCaseInfo "SpecificDay" $ test $ MkSpecificDay 2023 07 11 (MkDay 1)
    , testGroup "Appointment" [
          testCaseInfo "Weekly" $ test $ Weekly (MkDay 2)
        , testCaseInfo "Once"   $ test $ Once (MkSpecificDay 2024 11 11 (MkDay 1))
        ]
    ]

test :: forall a. ToDhall a => a -> IO String
test x = do
    -- This throws an exception if it doesn't typecheck, thus failing the test
    void $ Dhall.inputExpr (Text.pack expr)

    -- Return the expression tested to make the test output more informative
    return expr
  where
    bindings :: [(DatatypeName, String)]
    typ      :: String
    (typ, bindings) = flip runState [] $ toDhallType (Proxy @a)

    expr :: String
    expr = intercalate "\n" . concat $ [
        [ "let " <> t <> " = " <> d
        | (t, d) <- reverse (nubBy ((==) `on` fst) bindings)
        ]
      , [    ( if null bindings
                 then ""
                 else "in "
             )
          <> toDhallExpr x <> " : " <> typ
        ]
      ]
