{-# OPTIONS_GHC -fno-warn-orphans               #-}
{-# LANGUAGE CPP                                #-}
module Yage.Core.OpenGL (
	  module GL
    , module Yage.Core.OpenGL
	) where

import Yage.Prelude

import Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil           as GL hiding (texture3DWrap)
#ifdef GL_ERRCHECK
import qualified Graphics.GLUtil.GLError                 as GLE
#endif
import Data.Data

deriving instance Typeable GL.TextureFilter
deriving instance Data GL.TextureFilter
deriving instance Typeable GL.Repetition
deriving instance Data GL.Repetition
deriving instance Typeable GL.Clamping
deriving instance Data GL.Clamping

-- from GLUtil
texture3DWrap :: ParameterizedTextureTarget t => t -> StateVar (Repetition, Clamping)
texture3DWrap target = makeStateVar (get (textureWrapMode target S))
                             (forM_ [S,T,R] . aux)
  where aux x d = textureWrapMode target d $= x


checkError :: (MonadIO m) => String -> m ()
#ifdef GL_ERRCHECK
checkError = io . GLE.printErrorMsg
#else
checkError _ = return ()
#endif
{-# INLINE checkError #-}

checkErrorOf :: (MonadIO m) => String -> m a -> m a
#ifdef GL_ERRCHECK
checkErrorOf msg ma = do
    x <- ma
    io $ x `seq` GLE.throwErrorMsg msg
    return x
#else
checkErrorOf _ = id
#endif
{-# INLINE checkErrorOf #-}
