{-# LANGUAGE TemplateHaskell #-}

-- TODO: Tempoary
{-# OPTIONS_GHC -ddump-splices #-}

module Test.Sanity.TH (tests) where

import Test.Tasty.HUnit
import Test.Tasty
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Data.Proxy

import Generics.SOP.Haddocks
import Generics.SOP.Haddocks.TH

{-------------------------------------------------------------------------------
  Example 1
-------------------------------------------------------------------------------}

-- | This is the first example
data Example1 =
    Constr1   -- ^ First constructor of the first example
  | Constr2   -- ^ Second constructor of the first example
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

deriveHasHaddocks ''Example1

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.TH" [
      testCase "example1" test_example1
    ]

test_example1 :: Assertion
test_example1 =
    print $ haddocks (Proxy @Example1)


