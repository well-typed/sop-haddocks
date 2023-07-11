{-# LANGUAGE TemplateHaskell #-}

module Test.Sanity.TH (tests) where

import Test.Tasty.HUnit
import Test.Tasty
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Generics.SOP (Code)
import Data.SOP

import Generics.SOP.Haddocks.TH
import Generics.SOP.Haddocks qualified as Haddocks
import Generics.SOP.Haddocks (HasHaddocks(..), Haddocks)

{-------------------------------------------------------------------------------
  Example 1
-------------------------------------------------------------------------------}

-- | Example 1
data Example1 =
    E1_Constr1      -- ^ Example 1, constructor 1
  | E1_Constr2      -- ^ Example 1, constructor 2
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Example 2
data Example2 =
    E2_Constr1      -- ^ Example 2, only constructor
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Example 3
newtype Example3 =
    E3_Constr Int   -- ^ Example 3, newtype constructor
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Example 4
newtype Example4 =
    -- | Example 4, newtype constructor
    E4_Constr {
        e4_field :: Int  -- ^ Example 4, newtype record field
      }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Example 5
data Example5 =
    -- | Example 5, constructor 1
    E5_Constr1

    -- | Example 5, constructor 2
  | E5_Constr2 {
        e5_c2_field1 :: Int   -- ^ Example 5, constructor 2, field 1
      , e5_c2_field2 :: Char  -- ^ Example 5, constructor 2, field 2
      , e5_c2_field3 :: Bool  -- ^ Example 5, constructor 2, field 3
      }

    -- | Example 5, constructor 3
  | E5_Constr3 Int Bool

    -- | Example 5, constructor 4
  | E5_Constr4 {
        e5_c4_field1 :: Bool   -- ^ Example 5, constructor 4, field 1
      , e5_c4_field2 :: Int    -- ^ Example 5, constructor 4, field 2
      }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Example 6
data Example6 =
    -- | Example 5, constructor 1
    E6_Constr1

    -- | Example 5, constructor 2
  | E6_Constr2 {
        e6_c2_field1 :: Int   -- ^ Example 5, constructor 2, field 1
      , e6_c2_field2 :: Char  --   Example 5, constructor 2, field 2 (NOT HADDOCK)
      , e6_c2_field3 :: Bool  -- ^ Example 5, constructor 2, field 3
      }

    --   Example 5, constructor 3 (NOT HADDOCK)
  | E6_Constr3 Int Bool
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

deriveHasHaddocks ''Example1   -- Use the datatype name
deriveHasHaddocks 'E2_Constr1  -- Use the constructor name
deriveHasHaddocks ''Example3   -- Newtype
deriveHasHaddocks ''Example4   -- Newtype record
deriveHasHaddocks ''Example5   -- Regular record
deriveHasHaddocks ''Example6   -- Some haddocks missing

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.TH" [
      testCase "example1" $ test (Proxy @Example1) expected1
    , testCase "example2" $ test (Proxy @Example2) expected2
    , testCase "example3" $ test (Proxy @Example3) expected3
    , testCase "example4" $ test (Proxy @Example4) expected4
    , testCase "example5" $ test (Proxy @Example5) expected5
    , testCase "example5" $ test (Proxy @Example6) expected6
    ]
  where
    test :: HasHaddocks a => Proxy a -> Haddocks (Code a) -> Assertion
    test p expected = assertEqual "" expected $ haddocks p

{-------------------------------------------------------------------------------
  Expected haddocks

  The whitespace at the start of the haddocks is preserved; this looks a bit
  weird here, but for some applications whitespace might be relevant, so we
  leave any trimming up to client code.
-------------------------------------------------------------------------------}

expected1 :: Haddocks (Code Example1)
expected1 = Haddocks.ADT (Just " Example 1") $
       Haddocks.Constructor (Just " Example 1, constructor 1")
    :* Haddocks.Constructor (Just " Example 1, constructor 2")
    :* Nil

expected2 :: Haddocks (Code Example2)
expected2 = Haddocks.ADT (Just " Example 2") $
       Haddocks.Constructor (Just " Example 2, only constructor")
    :* Nil

expected3 :: Haddocks (Code Example3)
expected3 = Haddocks.Newtype (Just " Example 3") $
       Haddocks.Constructor (Just " Example 3, newtype constructor")

expected4 :: Haddocks (Code Example4)
expected4 = Haddocks.Newtype (Just " Example 4") $
     Haddocks.Record (Just " Example 4, newtype constructor") $
          Haddocks.FieldInfo (Just " Example 4, newtype record field")
       :* Nil

expected5 :: Haddocks (Code Example5)
expected5 = Haddocks.ADT (Just " Example 5") $
       Haddocks.Constructor (Just " Example 5, constructor 1")
    :* Haddocks.Record (Just " Example 5, constructor 2") (
            Haddocks.FieldInfo (Just " Example 5, constructor 2, field 1")
         :* Haddocks.FieldInfo (Just " Example 5, constructor 2, field 2")
         :* Haddocks.FieldInfo (Just " Example 5, constructor 2, field 3")
         :* Nil
         )
    :* Haddocks.Constructor (Just " Example 5, constructor 3")
    :* Haddocks.Record (Just " Example 5, constructor 4") (
            Haddocks.FieldInfo (Just " Example 5, constructor 4, field 1")
         :* Haddocks.FieldInfo (Just " Example 5, constructor 4, field 2")
         :* Nil
         )
    :* Nil

expected6 :: Haddocks (Code Example6)
expected6 = Haddocks.ADT (Just " Example 6") $
       Haddocks.Constructor (Just " Example 5, constructor 1")
    :* Haddocks.Record (Just " Example 5, constructor 2") (
            Haddocks.FieldInfo (Just " Example 5, constructor 2, field 1")
         :* Haddocks.FieldInfo Nothing
         :* Haddocks.FieldInfo (Just " Example 5, constructor 2, field 3")
         :* Nil
         )
    :* Haddocks.Constructor Nothing
    :* Nil
