module Yage.Rendering.Textures.MipMapChain
    ( MipMapChain, mkMipMapChain, hasMipMaps
    ) where

import           Prelude                             ( not, Bool, (.), null, ($) )
import           Data.NonNull                        ( NonNull, nonNull, tail )

-- | The first element in the 'MipMapChain' is the base element
-- (e.g. 'Texture' or 'FilePath'). Every 'MipMapChain' has a base
-- element (aka is never empty). The other elements in the chain are the mipmap
-- levels in resoloution descending order.
type MipMapChain tex = NonNull [tex]

-- | creates a mipmap chain from a base and a mip map list
--   TODO : check correct resoulutions
mkMipMapChain :: tex -> [tex] -> MipMapChain tex
mkMipMapChain base mipmaps = nonNull $ base:mipmaps

-- | Predicate if a `MipMapChain` has destinct mipmap levels beside the base (0th)
hasMipMaps :: MipMapChain a -> Bool
hasMipMaps = not . null . tail
{-# INLINE hasMipMaps #-}
