-- | Get access to documentation strings
--
-- Intended for qualified import.
--
-- > import Generics.SOP.Haddocks.TH
-- > import Generics.SOP.Haddocks qualified as Haddocks
-- > import Generics.SOP.Haddocks (HasHaddocks(..))
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

-- | Haddocks associated with a datatype
data Haddocks :: [[Type]] -> Type where
  -- | Standard algebraic datatype
  ADT ::
       Doc
    -> Haddocks xss

  -- | Newtype
  Newtype ::
       Doc
    -> Haddocks '[ '[x] ]

deriving instance Show (Haddocks xss)

{-------------------------------------------------------------------------------
  Class
-------------------------------------------------------------------------------}

class Generic a => HasHaddocks a where
  haddocks :: proxy a -> Haddocks (Code a)


