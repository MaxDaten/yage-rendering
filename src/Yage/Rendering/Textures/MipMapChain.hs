module Yage.Rendering.Textures.MipMapChain
    ( MipMapChain, mkMipMapChain, hasMipMaps
    ) where

import           Prelude                             ( not, Bool, (.), null, ($) )
import           Data.NonNull                        ( NonNull, nonNull, tail )

type MipMapChain tex = NonNull [tex]

-- | creates a mipmap chain from a base and a mip map list
--   TODO : check correct resoulutions
mkMipMapChain :: tex -> [tex] -> MipMapChain tex
mkMipMapChain base mipmaps = nonNull $ base:mipmaps

-- | Predicate if a `MipMapChain` has destinct mipmap levels beside the base (0th)
hasMipMaps :: MipMapChain a -> Bool
hasMipMaps = not . null . tail
{-# INLINE hasMipMaps #-}
