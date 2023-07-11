{-# LANGUAGE CPP #-}

module Generics.SOP.Haddocks.Util (
    -- * th-abstraction
    isNewtypeVariant
  ) where

import Language.Haskell.TH.Datatype

{-------------------------------------------------------------------------------
   th-abstraction
-------------------------------------------------------------------------------}

-- | Does this variant correspond to a newtype?
--
-- Shamelessly stolen from @generics-sop@
isNewtypeVariant :: DatatypeVariant -> Bool
isNewtypeVariant Datatype        = False
isNewtypeVariant DataInstance    = False
isNewtypeVariant Newtype         = True
isNewtypeVariant NewtypeInstance = True
#if MIN_VERSION_th_abstraction(0,5,0)
isNewtypeVariant TypeData        = False
#endif
