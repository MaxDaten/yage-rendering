{-# LANGUAGE ExistentialQuantification, Rank2Types, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, StandaloneDeriving, TypeFamilies #-}
module Yage.Rendering.Shader where

import             Yage.Prelude
import qualified   Prelude
import             Control.Monad.Trans

import             Graphics.GLUtil               (ShaderProgram)
import             Graphics.GLUtil.Linear        (AsUniform)

---------------------------------------------------------------------------------------------------


--data ShaderAttributeDef vad = ShaderAttributeDef
--    { shaderAttr'name       :: String
--    , shaderAttr'descriptor :: vad
--    }

--data ShaderUniformDef r s m = ShaderUniformDef
--    { uniformFun :: r -> s -> m () }

--data ShaderDef vad r s = ShaderDef
--    { attribs :: [(String, vad)]        -- | attribute name asign to vertex array descriptor
--    , uniform :: r -> s -> IO ()
--    }

---------------------------------------------------------------------------------------------------

--(.=) :: (AsUniform u, Monad m, MonadShader sdf sp m) => (sdf -> UniformDef u m) -> u -> m ()
--(.=) = setUniform
