{-# LANGUAGE TemplateHaskell #-}

-- TODO: Temporary
{-# OPTIONS_GHC -ddump-splices #-}

module Test.Sanity.TH (tests) where

import Test.Tasty.HUnit
import Test.Tasty
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Data.Proxy

import Generics.SOP.Haddocks.TH
import Generics.SOP.Haddocks (HasHaddocks(..))

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

deriveHasHaddocks ''Example1   -- Use the datatype name
deriveHasHaddocks 'E2_Constr1  -- Use the constructor name
deriveHasHaddocks ''Example3   -- Newtype
deriveHasHaddocks ''Example4   -- Newtype record
deriveHasHaddocks ''Example5   -- Regular record

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.TH" [
      testCase "example1" test_example1
    , testCase "example2" test_example2
    , testCase "example3" test_example3
    , testCase "example4" test_example4
    , testCase "example5" test_example5
    ]

test_example1 :: Assertion
test_example1 =
    print $ haddocks (Proxy @Example1)

test_example2 :: Assertion
test_example2 =
    print $ haddocks (Proxy @Example2)

test_example3 :: Assertion
test_example3 =
    print $ haddocks (Proxy @Example3)

test_example4 :: Assertion
test_example4 =
    print $ haddocks (Proxy @Example4)

test_example5 :: Assertion
test_example5 =
    print $ haddocks (Proxy @Example5)
