module Yage.Rendering.Textures.MipMapChain
    ( MipMapChain, nonNull, hasMipMaps
    ) where

import           Prelude                             ( not, Bool, (.), null )
import           Data.NonNull                        ( NonNull, nonNull, tail )

type MipMapChain tex = NonNull [tex]

-- | Predicate if a `MipMapChain` has destinct mipmap levels beside the base (0th)
hasMipMaps :: MipMapChain a -> Bool
hasMipMaps = not . null . tail
{-# INLINE hasMipMaps #-}
