module Yage.Rendering.Textures.MipMapChain
    ( MipMapChain, mkMipMapChain, hasMipMaps, mipMapBase
    ) where

import           Prelude                             ( not, Bool, (.), null )
import           Data.List.NonEmpty

-- | The first element in the 'MipMapChain' is the base element
-- (e.g. 'Texture' or 'FilePath'). Every 'MipMapChain' has a base
-- element (aka is never empty see: 'NonEmpty'). The other elements in the chain are the mipmap
-- levels in resoloution descending order.
type MipMapChain tex = NonEmpty tex

-- | creates a mipmap chain from a base and a mip map list
--   TODO : check correct resoulutions
mkMipMapChain :: tex -> [tex] -> MipMapChain tex
mkMipMapChain base mipmaps = base :| mipmaps

-- | Predicate if a `MipMapChain` has destinct mipmap levels beside the base (0th)
hasMipMaps :: MipMapChain a -> Bool
hasMipMaps = not . null . tail
{-# INLINE hasMipMaps #-}

mipMapBase :: MipMapChain tex -> tex
mipMapBase = head
{-# INLINE mipMapBase #-}
