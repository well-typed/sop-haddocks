-- | Get access to documentation strings
--
-- Intended for qualified import.
--
-- > import Generics.SOP.Haddocks.TH
-- > import Generics.SOP.Haddocks qualified as Haddocks
-- > import Generics.SOP.Haddocks (HasHaddocks(..), Haddocks)
module Generics.SOP.Haddocks (
    -- * Definition
    Haddocks(..)
  , ConstructorInfo(..)
  , FieldInfo(..)
  , Doc
    -- * Class
  , HasHaddocks(..)
  ) where

import GHC.Show
import Generics.SOP (Generic, Code)
import Data.SOP
import Data.Kind
import Data.SOP.Dict

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Doc = Maybe String

-- | Haddocks associated with a datatype
data Haddocks :: [[Type]] -> Type where
  -- | Standard algebraic datatype
  ADT ::
       Doc
    -> NP ConstructorInfo xss
    -> Haddocks xss

  -- | Newtype
  Newtype ::
       Doc
    -> ConstructorInfo '[x]
    -> Haddocks '[ '[x] ]

data ConstructorInfo :: [Type] -> Type where
  -- | Normal (or infix) constructor
  Constructor :: SListI xs => Doc -> ConstructorInfo xs

  -- | Record constructor
  Record :: SListI xs => Doc -> NP FieldInfo xs -> ConstructorInfo xs

data FieldInfo :: Type -> Type where
  FieldInfo :: Doc -> FieldInfo x

{-------------------------------------------------------------------------------
  'Show' instances

  We write these out by hand so that we can take advantage of
  'dictShowConstructors' and co; without that, we would get much less clean
  instances (they would have unnecessary constraints on them).
-------------------------------------------------------------------------------}

instance Show (Haddocks xss) where
  showsPrec p (ADT doc constrs)
    | Dict <- dictShowConstructors constrs
    = showParen (p >= appPrec1) $
          showString "ADT "
        . showsPrec appPrec1 doc
        . showSpace
        . showsPrec appPrec1 constrs
  showsPrec p (Newtype doc constr)
    = showParen (p >= appPrec1) $
          showString "Newtype "
        . showsPrec appPrec1 doc
        . showSpace
        . showsPrec appPrec1 constr

instance Show (ConstructorInfo xs) where
  showsPrec p (Constructor doc)
    = showParen (p >= appPrec1) $
          showString "Constructor"
        . showSpace
        . showsPrec appPrec1 doc
  showsPrec p (Record doc fields)
    | Dict <- dictShowFields fields
    = showParen (p >= appPrec1) $
          showString "Record"
        . showSpace
        . showsPrec appPrec1 doc
        . showSpace
        . showsPrec appPrec1 fields

deriving instance Show (FieldInfo x)

dictShowConstructors :: NP f xss -> Dict (All (Compose Show ConstructorInfo)) xss
dictShowConstructors =
    all_NP . go
  where
    -- Explicit recursion avoids an 'SListI' constraint
    go :: NP f xss -> NP (Dict (Compose Show ConstructorInfo)) xss
    go Nil       = Nil
    go (_ :* xs) = Dict :* go xs

dictShowFields :: NP f xs -> Dict (All (Compose Show FieldInfo)) xs
dictShowFields =
    all_NP . go
  where
    go :: NP f xs -> NP (Dict (Compose Show FieldInfo)) xs
    go Nil       = Nil
    go (_ :* xs) = Dict :* go xs

{-------------------------------------------------------------------------------
  'Eq' instances
-------------------------------------------------------------------------------}

instance Eq (Haddocks xss) where
  ADT doc constrs == ADT doc' constrs'
    | Dict <- dictEqConstructors constrs
    = doc == doc' && constrs == constrs'
  Newtype doc constr == Newtype doc' constr'
    = doc == doc' && constr == constr'
  _ == _
    = False

instance Eq (ConstructorInfo xs) where
  Constructor doc == Constructor doc'
    = doc == doc'
  Record doc fields == Record doc' fields'
    | Dict <- dictEqFields fields
    = doc == doc' && fields == fields'
  _ == _
    = False

dictEqConstructors :: NP f xs -> Dict (All (Compose Eq ConstructorInfo)) xs
dictEqConstructors =
    all_NP . go
  where
    go :: NP f xs -> NP (Dict (Compose Eq ConstructorInfo)) xs
    go Nil       = Nil
    go (_ :* xs) = Dict :* go xs

dictEqFields :: NP f xs -> Dict (All (Compose Eq FieldInfo)) xs
dictEqFields =
    all_NP . go
  where
    go :: NP f xs -> NP (Dict (Compose Eq FieldInfo)) xs
    go Nil       = Nil
    go (_ :* xs) = Dict :* go xs

deriving instance Eq (FieldInfo x)

{-------------------------------------------------------------------------------
  Class
-------------------------------------------------------------------------------}

class Generic a => HasHaddocks a where
  haddocks :: proxy a -> Haddocks (Code a)


