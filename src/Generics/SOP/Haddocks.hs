-- | Get access to documentation strings
--
-- Intended for qualified import.
--
-- > import Generics.SOP.Haddocks qualified as Haddocks
module Generics.SOP.Haddocks (
    -- * Definition
    Haddocks(..)
  , Doc
    -- * Class
  , HasHaddocks(..)
  ) where

import Data.Kind
import Generics.SOP

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Doc = Maybe String

data Haddocks :: [[Type]] -> Type where
  ADT :: Doc -> Haddocks xss

deriving instance Show (Haddocks xss)

{-------------------------------------------------------------------------------
  Class
-------------------------------------------------------------------------------}

class Generic a => HasHaddocks a where
  haddocks :: proxy a -> Haddocks (Code a)


