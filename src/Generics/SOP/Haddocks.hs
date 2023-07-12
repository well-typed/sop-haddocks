-- | Get access to documentation strings
--
-- Intended for qualified import.
--
-- > import Generics.SOP.Haddocks.TH
-- > import Generics.SOP.Haddocks qualified as Haddocks
-- > import Generics.SOP.Haddocks (HasHaddocks(..), Haddocks(..))
module Generics.SOP.Haddocks (
    -- * Class
    HasHaddocks(..)
    -- * Definition
  , Haddocks(..)
  , Constructor(..)
  , Argument(..)
  , Doc
  ) where

import GHC.Show
import Generics.SOP (Generic, Code)
import Data.SOP
import Data.Kind
import Data.SOP.Dict

{-------------------------------------------------------------------------------
  Class
-------------------------------------------------------------------------------}

class Generic a => HasHaddocks a where
  getHaddocks :: proxy a -> Haddocks (Code a)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Doc = Maybe String

-- | Haddocks associated with a datatype
data Haddocks :: [[Type]] -> Type where
  Haddocks :: Doc -> NP Constructor xss -> Haddocks xss

-- | Haddocks associated with a constructor of a datatype
data Constructor :: [Type] -> Type where
  -- | Constructor
  --
  -- This could be a regular constructor, an infix constructor, or a record
  -- constructor; in all three cases haddocks can be attached to the constructor
  -- arguments.
  --
  -- If you need to distinguish between these cases, you can use the regular
  -- metadata from @generics-sop@.
  Constructor :: SListI xs => Doc -> NP Argument xs -> Constructor xs

-- | Haddocks associated with an argument of a constructor
--
-- This could be a field of a record or an unnamed argument to a constructor;
-- in both cases, documentation strings can be attached to the argument
-- <https://haskell-haddock.readthedocs.io/en/latest/markup.html#constructors-and-record-fields>.
data Argument :: Type -> Type where
  Argument :: Doc -> Argument x

{-------------------------------------------------------------------------------
  'Show' instances

  We write these out by hand so that we can take advantage of
  'dictShowConstructors' and co; without that, we would get much less clean
  instances (they would have unnecessary constraints on them).
-------------------------------------------------------------------------------}

instance Show (Haddocks xss) where
  showsPrec p (Haddocks doc constrs)
    | Dict <- dictShowConstructors constrs
    = showParen (p >= appPrec1) $
          showString "Haddocks "
        . showsPrec appPrec1 doc
        . showSpace
        . showsPrec appPrec1 constrs

instance Show (Constructor xs) where
  showsPrec p (Constructor doc fields)
    | Dict <- dictShowArguments fields
    = showParen (p >= appPrec1) $
          showString "Constructor"
        . showSpace
        . showsPrec appPrec1 doc
        . showSpace
        . showsPrec appPrec1 fields

deriving instance Show (Argument x)

dictShowConstructors :: NP f xss -> Dict (All (Compose Show Constructor)) xss
dictShowConstructors =
    all_NP . go
  where
    -- Explicit recursion avoids an 'SListI' constraint
    go :: NP f xss -> NP (Dict (Compose Show Constructor)) xss
    go Nil       = Nil
    go (_ :* xs) = Dict :* go xs

dictShowArguments :: NP f xs -> Dict (All (Compose Show Argument)) xs
dictShowArguments =
    all_NP . go
  where
    go :: NP f xs -> NP (Dict (Compose Show Argument)) xs
    go Nil       = Nil
    go (_ :* xs) = Dict :* go xs

{-------------------------------------------------------------------------------
  'Eq' instances
-------------------------------------------------------------------------------}

instance Eq (Haddocks xss) where
  Haddocks doc constrs == Haddocks doc' constrs'
    | Dict <- dictEqConstructors constrs
    = doc == doc' && constrs == constrs'

instance Eq (Constructor xs) where
  Constructor doc fields == Constructor doc' fields'
    | Dict <- dictEqFields fields
    = doc == doc' && fields == fields'

dictEqConstructors :: NP f xs -> Dict (All (Compose Eq Constructor)) xs
dictEqConstructors =
    all_NP . go
  where
    go :: NP f xs -> NP (Dict (Compose Eq Constructor)) xs
    go Nil       = Nil
    go (_ :* xs) = Dict :* go xs

dictEqFields :: NP f xs -> Dict (All (Compose Eq Argument)) xs
dictEqFields =
    all_NP . go
  where
    go :: NP f xs -> NP (Dict (Compose Eq Argument)) xs
    go Nil       = Nil
    go (_ :* xs) = Dict :* go xs

deriving instance Eq (Argument x)

