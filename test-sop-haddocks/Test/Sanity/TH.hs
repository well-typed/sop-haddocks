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

-- | This is the first example
data Example1 =
    E1_Constr1      -- ^ First constructor of the first example
  | E1_Constr2      -- ^ Second constructor of the first example
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | This is the second example
data Example2 =
    E2_Constr1      -- ^ First constructor of the second example
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | This is the third example
newtype Example3 =
    E3_Constr Int   -- ^ Constructor of the third example
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

deriveHasHaddocks ''Example1   -- Use the datatype name
deriveHasHaddocks 'E2_Constr1  -- Use the constructor name
deriveHasHaddocks ''Example3   -- Newtype

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.TH" [
      testCase "example1" test_example1
    , testCase "example2" test_example2
    , testCase "example3" test_example3
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

