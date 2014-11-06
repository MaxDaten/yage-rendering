module Yage.Rendering.Textures.MipMapChain
    ( MipMapChain, mkMipMapChain, mipMapChain, hasMipMaps, mipMapBase, maxMipMapLevel
    ) where

import           Prelude                             ( not, Bool, Maybe, Int, (-), (.), null )
import           Data.List.NonEmpty

-- | The first element in the 'MipMapChain' is the base element
-- (e.g. 'Texture' or 'FilePath'). Every 'MipMapChain' has a base
-- element (aka is never empty see: 'NonEmpty'). The other elements in the chain are the mipmap
-- levels in resoloution descending order.
type MipMapChain tex = NonEmpty tex

-- | creates a mipmap chain from a base and a mip map list
mkMipMapChain :: tex -> [tex] -> MipMapChain tex
mkMipMapChain base mipmaps = base :| mipmaps
{-# INLINE mkMipMapChain #-}

-- | creates a 'MipMapChain' from a list. At least one element as the base element
-- is required. On an empty list 'Nothing' is returned.
mipMapChain :: [tex] -> Maybe (MipMapChain tex)
mipMapChain = nonEmpty
{-# INLINE mipMapChain #-}

-- | Predicate if a `MipMapChain` has destinct mipmap levels beside the base (0th)
hasMipMaps :: MipMapChain a -> Bool
hasMipMaps = not . null . tail
{-# INLINE hasMipMaps #-}

mipMapBase :: MipMapChain tex -> tex
mipMapBase = head
{-# INLINE mipMapBase #-}

maxMipMapLevel :: MipMapChain tex -> Int
maxMipMapLevel mips = length mips - 1
{-# INLINE maxMipMapLevel #-}
