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
import Generics.SOP.Haddocks (HasHaddocks(..), Haddocks(..))

{-------------------------------------------------------------------------------
  Example 1
-------------------------------------------------------------------------------}

-- | Example 1
data Example1 =
    E1_Constr1      -- ^ Example 1, constr 1
  | E1_Constr2      -- ^ Example 1, constr 2
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Example 2
data Example2 =
    E2_Constr1      -- ^ Example 2, only constr
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Example 3
newtype Example3 =
    E3_Constr Int   -- ^ Example 3, newtype constr
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Example 4
newtype Example4 =
    -- | Example 4, newtype constr
    E4_Constr {
        e4_field :: Int  -- ^ Example 4, newtype record field
      }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Example 5
data Example5 =
    -- | Example 5, constr 1
    E5_Constr1

    -- | Example 5, constr 2
  | E5_Constr2 {
        e5_c2_field1 :: Int   -- ^ Example 5, constr 2, field 1
      , e5_c2_field2 :: Char  -- ^ Example 5, constr 2, field 2
      , e5_c2_field3 :: Bool  -- ^ Example 5, constr 2, field 3
      }

    -- | Example 5, constr 3
  | E5_Constr3 Int Bool

    -- | Example 5, constr 4
  | E5_Constr4 {
        e5_c4_field1 :: Bool   -- ^ Example 5, constr 4, field 1
      , e5_c4_field2 :: Int    -- ^ Example 5, constr 4, field 2
      }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Example 6
data Example6 =
    -- | Example 6, constr 1
    E6_Constr1

    -- | Example 6, constr 2
  | E6_Constr2 {
        e6_c2_field1 :: Int   -- ^ Example 6, constr 2, field 1
      , e6_c2_field2 :: Char  --   Example 6, constr 2, field 2 (NOT HADDOCK)
      , e6_c2_field3 :: Bool  -- ^ Example 6, constr 2, field 3
      }

    --   Example 6, constr 3 (NOT HADDOCK)
  | E6_Constr3 Int Bool
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Example 7
data Example7 =
    E7_Constr1   -- ^ Example 7, first constr
      Int        -- ^ Example 7, first constr, first arg
      Bool       -- ^ Example 7, first constr, second arg

  | (:**)        -- ^ Example 7, second (infix) constr
      Char       -- ^ Example 7, second constr, left arg
      Int        -- ^ Example 7, second constr, right arg
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | Example 8
newtype Example8 =
    E8_Constr   -- ^ Example 8, newtype constr constr
      Int       -- ^ Example 8, newtype arg
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

deriveHasHaddocks ''Example1   -- Use the datatype name
deriveHasHaddocks 'E2_Constr1  -- Use the constr name
deriveHasHaddocks ''Example3   -- Newtype
deriveHasHaddocks ''Example4   -- Newtype record
deriveHasHaddocks ''Example5   -- Regular record
deriveHasHaddocks ''Example6   -- Some haddocks missing
deriveHasHaddocks ''Example7   -- Positional arguments, normal datatype
deriveHasHaddocks ''Example8   -- Positional arguments, newtype

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
    , testCase "example6" $ test (Proxy @Example6) expected6
    , testCase "example7" $ test (Proxy @Example7) expected7
    , testCase "example8" $ test (Proxy @Example8) expected8
    ]
  where
    test :: HasHaddocks a => Proxy a -> Haddocks (Code a) -> Assertion
    test p expected = assertEqual "" expected $ getHaddocks p

{-------------------------------------------------------------------------------
  Expected haddocks

  The whitespace at the start of the haddocks is preserved; this looks a bit
  weird here, but for some applications whitespace might be relevant, so we
  leave any trimming up to client code.
-------------------------------------------------------------------------------}

expected1 :: Haddocks (Code Example1)
expected1 = Haddocks (Just " Example 1") $
       Haddocks.Constructor (Just " Example 1, constr 1") Nil
    :* Haddocks.Constructor (Just " Example 1, constr 2") Nil
    :* Nil

expected2 :: Haddocks (Code Example2)
expected2 = Haddocks (Just " Example 2") $
       Haddocks.Constructor (Just " Example 2, only constr") Nil
    :* Nil

expected3 :: Haddocks (Code Example3)
expected3 = Haddocks (Just " Example 3") $
       Haddocks.Constructor (Just " Example 3, newtype constr") (
            Haddocks.Argument Nothing
         :* Nil
         )
    :* Nil

expected4 :: Haddocks (Code Example4)
expected4 = Haddocks (Just " Example 4") $
       Haddocks.Constructor (Just " Example 4, newtype constr") (
            Haddocks.Argument (Just " Example 4, newtype record field")
         :* Nil
         )
    :* Nil

expected5 :: Haddocks (Code Example5)
expected5 = Haddocks (Just " Example 5") $
       Haddocks.Constructor (Just " Example 5, constr 1") Nil
    :* Haddocks.Constructor (Just " Example 5, constr 2") (
            Haddocks.Argument (Just " Example 5, constr 2, field 1")
         :* Haddocks.Argument (Just " Example 5, constr 2, field 2")
         :* Haddocks.Argument (Just " Example 5, constr 2, field 3")
         :* Nil
         )
    :* Haddocks.Constructor (Just " Example 5, constr 3") (
            Haddocks.Argument Nothing
         :* Haddocks.Argument Nothing
         :* Nil
         )
    :* Haddocks.Constructor (Just " Example 5, constr 4") (
            Haddocks.Argument (Just " Example 5, constr 4, field 1")
         :* Haddocks.Argument (Just " Example 5, constr 4, field 2")
         :* Nil
         )
    :* Nil

expected6 :: Haddocks (Code Example6)
expected6 = Haddocks (Just " Example 6") $
       Haddocks.Constructor (Just " Example 6, constr 1") Nil
    :* Haddocks.Constructor (Just " Example 6, constr 2") (
            Haddocks.Argument (Just " Example 6, constr 2, field 1")
         :* Haddocks.Argument Nothing
         :* Haddocks.Argument (Just " Example 6, constr 2, field 3")
         :* Nil
         )
    :* Haddocks.Constructor Nothing (
            Haddocks.Argument Nothing
         :* Haddocks.Argument Nothing
         :* Nil
         )
    :* Nil

expected7 :: Haddocks (Code Example7)
expected7 = Haddocks (Just " Example 7") $
       Haddocks.Constructor (Just " Example 7, first constr") (
            Haddocks.Argument (Just " Example 7, first constr, first arg")
         :* Haddocks.Argument (Just " Example 7, first constr, second arg")
         :* Nil
         )
    :* Haddocks.Constructor (Just " Example 7, second (infix) constr") (
            Haddocks.Argument (Just " Example 7, second constr, left arg")
         :* Haddocks.Argument (Just " Example 7, second constr, right arg")
         :* Nil
         )
    :* Nil

expected8 :: Haddocks (Code Example8)
expected8 = Haddocks (Just " Example 8") $
       Haddocks.Constructor (Just " Example 8, newtype constr constr") (
            Haddocks.Argument (Just " Example 8, newtype arg")
         :* Nil
         )
    :* Nil

