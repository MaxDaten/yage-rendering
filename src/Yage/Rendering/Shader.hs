{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell              #-}
{-# LANGUAGE ConstraintKinds              #-}
{-# LANGUAGE KindSignatures               #-}
{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE TypeOperators                #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE UndecidableInstances         #-} -- FIXME : should not be neccessary
module Yage.Rendering.Shader
    ( module Yage.Rendering.Shader
    , module VinylGL
    , module U
    , Symbol, KnownSymbol
    , ShaderType(..)
    ) where

import           Yage.Prelude
import           Yage.Lens

import           GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import           Data.Proxy

import           Data.Vinyl                as VinylGL
import           Data.Vinyl.Universe       as U
import qualified Data.Map.Strict           as M ( fromList )

import qualified Data.ByteString           as BS

import           Control.Exception         (ErrorCall)

import           Graphics.VinylGL.Uniforms as VinylGL hiding (setUniforms)

import qualified Graphics.VinylGL.Uniforms as V
import qualified Data.Vinyl.Idiom.Identity as I
import           Text.Regex
import           Text.Regex.Posix
import           Text.Read                 ( read )


import           Yage.Core.OpenGL hiding (Shader, shaderSource, loadShaderProgramWith, Proxy)

import           Yage.Rendering.Resources.ResTypes

class HasTextures t where
    getTextures :: t -> [Texture]

instance HasTextures Texture where
    getTextures t = [t]

instance HasTextures [Texture] where
    getTextures = id

instance HasTextures (I.Identity Texture) where
    getTextures (I.Identity t) = getTextures t

instance HasTextures (FieldRec f '[]) where
    getTextures = const []

instance ( HasTextures (Textures ts), HasTextures t )
    => HasTextures ( Textures ( ( (sy::Symbol) ::: t ) ': ts ) ) where
    getTextures (I.Identity t :& ts) = getTextures t ++ getTextures ts

-- | assign each Texture to array-fields
-- "sampler2d sampler" start with zero, bracket with idx is appended
-- "sampler2d sampler[10]" the array bracket contains the start idx, here '10'
unrollArrayLocations :: (String, [Texture]) -> [(String, Texture)]
unrollArrayLocations (name, [tex]) = [(name, tex)]
unrollArrayLocations (name, texs) =
    -- match against the last array idx in brackets
    case name =~ lastBracketPat :: String of
        ""          -> zipWith ( \(idx::Int) -> (name ++ arrayIdxBracket idx,) ) [0..] texs
        theBracket  ->
            let startIdx        :: Int
                startIdx        = read ( theBracket =~ ("[0-9]+" :: String) )
                replaceIdx idx  = subRegex (mkRegex lastBracketPat) name (arrayIdxBracket idx)
            in zipWith ( \(idx::Int) tex -> (replaceIdx idx, tex) ) [startIdx ..] texs
    where
    lastBracketPat :: String
    lastBracketPat = "\\[[0-9]+\\]$"
    arrayIdxBracket idx = "[" ++ show idx ++ "]"

class HasTextureFields ts where
    textureFields :: ts -> [(String, Texture)]

instance HasTextureFields (Textures '[]) where
    textureFields = const []

instance ( HasTextureFields (Textures ts), HasTextures t, KnownSymbol sy )
    => HasTextureFields ( Textures ( ( (sy::Symbol) ::: t ) ': ts ) ) where

    textureFields (I.Identity x :& xs) =
        unrollArrayLocations (symbolVal (Proxy::Proxy sy), getTextures x) ++ textureFields xs


type IsShaderData us ts   = ( UniformFields (Uniforms us)
                            , HasTextureFields (Textures ts)
                            )

type Uniforms us = PlainFieldRec us
type Textures ts = PlainFieldRec ts

data ShaderData uniforms textures = ShaderData
    { _shaderUniforms :: Uniforms uniforms
    , _shaderTextures :: Textures textures
    }

makeLenses ''ShaderData


data ShaderSource = ShaderSource
    { _srcDebugName   :: !String
    -- ^ just for debug
    , _srcType        :: !ShaderType
    , _srcRaw         :: !ByteString
    -- ^ strict `ByteString`
    } deriving (Show, Eq, Ord)

makeLenses ''ShaderSource

compilationUnit :: Getter ShaderSource (ShaderType, ByteString)
compilationUnit = to unit where
    unit src = (src^.srcType, src^.srcRaw)

instance Hashable ShaderSource where
    hashWithSalt salt ShaderSource{_srcRaw} =
        salt `hashWithSalt` _srcRaw


data ShaderProgramUnit = ShaderProgramUnit
    { _shaderName    :: !String
    , _shaderSources :: [ShaderSource]
    } deriving (Show, Eq, Ord)

makeLenses ''ShaderProgramUnit


data ShaderUnit p d v = ShaderUnit
    { _shaderProgram    :: p
    } deriving (Show, Eq, Ord)

makeLenses ''ShaderUnit


type Shader u t v = ShaderUnit ShaderProgramUnit (ShaderData u t) v

class HasShaderSource t where
    shaderSource :: Getter t ShaderSource

---------------------------------------------------------------------------------------------------


type TextureSampler u = (u::Symbol) ::: Texture
type TextureArray u = (u::Symbol) ::: [ Texture ]


deriving instance Show ShaderProgram


instance (Monoid (Uniforms u), Monoid (Textures t)) => Monoid (ShaderData u t) where
    mempty = ShaderData mempty mempty
    (ShaderData u t) `mappend` (ShaderData u' t') = ShaderData (mappend u u') (mappend t t')


append :: ShaderData u t -> ShaderData u' t' -> ShaderData (u ++ u') (t ++ t')
append (ShaderData u t) (ShaderData u' t') = ShaderData (u <+> u') (t <+> t')


-- | Set GLSL uniform parameters from a 'PlainRec' representing a
-- subset of all uniform parameters used by a program.
{--
setUniforms :: forall ts. UniformFields (PlainFieldRec ts)
            => ShaderProgram -> PlainFieldRec ts -> IO ()
setUniforms s x = case checks of
                    Left msg -> error msg
                    Right _  -> V.setUniformFields locs x
  where fnames = fieldNames (undefined::PlainFieldRec ts)
        checks = do namesCheck "GLSL program" fnames (M.keys $ uniforms s)
                    typesCheck False fieldTypes (snd <$> uniforms s)
        fieldTypes = M.fromList $
                     zip fnames (fieldGLTypes (undefined::PlainFieldRec ts))
        locs = map (fmap fst . (`M.lookup` uniforms s)) fnames
{-# INLINE setUniforms #-}
--}

setUniforms :: forall ts. UniformFields (Uniforms ts)
            => ShaderProgram -> Uniforms ts -> IO ()
setUniforms s x =
    V.setUniforms s x
        `catch` \(e::ErrorCall) -> error . show $ format "setUniforms: {}: {}" (Shown e, Shown s)


-- stolen from here: https://github.com/acowley/GLUtil/blob/master/src/Graphics/GLUtil/ShaderProgram.hs
-- TODO: integrate it back to package (getActives for arrays)
-- {

-- | Load a 'ShaderProgram' from a list of individual shader program
-- source strings. The active attributes and uniforms in the linked program are
-- recorded in the 'ShaderProgram'
loadShaderProgram :: [(ShaderType, BS.ByteString)] -> IO ShaderProgram
loadShaderProgram = flip loadShaderProgramWith (const (return ()))


-- | Helper for @load*Program*@ variants.
loadShaderProgramWith :: [(ShaderType, BS.ByteString)] -> (Program -> IO ())
                       -> IO ShaderProgram
loadShaderProgramWith sources m =
  do p <- mapM (uncurry $ loadShaderBS "BB Lit") sources >>= flip linkShaderProgramWith m
     throwError
     (attrs,unis) <- getActives p
     return $ ShaderProgram (M.fromList attrs) (M.fromList unis) p


-- | Get all attributes and uniforms used by a program. Note that
-- unused parameters may be elided by the compiler, and so will not be
-- considered as active.
--
-- for this example definition OpenGL 4.1 delivers following locations:
-- @
-- struct Tex
-- {
--     vec2 uv;
--     vec2 size;
-- };
-- uniform Tex textureData[2];
-- @
-- locations from OpenGL:
-- @
-- textureData[0].uv[0] // location: 0
-- textureData[0].size  // location: 2
-- textureData[1].uv[0] // location: 3
-- textureData[1].size  // location: 5
-- @
-- note the missing uv[1] locations
--
-- corresponding OpenGL Spec:
-- https://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetActiveUniform.xml
--
-- Uniform variables that are declared as structures
-- or arrays of structures will not be returned directly
-- by this function. Instead, each of these uniform
-- variables will be reduced to its fundamental components
-- containing the "." and "[]" operators such that each
-- of the names is valid as an argument to
-- glGetUniformLocation. Each of these reduced uniform
-- variables is counted as one active uniform variable
-- and is assigned an index. A valid name cannot be a
-- structure, an array of structures, or a subcomponent
-- of a vector or matrix.
--
-- The size of the uniform variable will be returned in
-- size. Uniform variables other than arrays will have
-- a size of 1. Structures and arrays of structures
-- will be reduced as described earlier, such that each
-- of the names returned will be a data type in the
-- earlier list. If this reduction results in an
-- array, the size returned will be as described for
-- uniform arrays; otherwise, the size returned will be 1.
getActives :: Program ->
              IO ( [(String, (AttribLocation, VariableType))]
                 , [(String, (UniformLocation, VariableType))] )
getActives p =
  (,) <$> ( get ( activeAttribs p ) >>= mapM ( aux ( attribLocation p ) ) )
      <*> liftM concat ( get ( activeUniforms p ) >>= mapM unifromLocationExpandArrays )
  where aux f (_,t,name) = get (f name) >>= \l -> return (name, (l, t))

        unifromLocationExpandArrays :: (GLint, VariableType, String) -> IO [(String, (UniformLocation, VariableType))]
        unifromLocationExpandArrays (size, t, name)
            | size == 1 && not (isArrayName name) =
                get( uniformLocation p name) >>= \l -> return [(name, (l, t))]
            | size >= 1 && isArrayName name       =
                mapM (\n -> (n,) <$> get (uniformLocation p n)) (expandArrayNames name size)
                >>= mapM (\(n,l) -> return (n, (l, t)))
            | otherwise = error "implementation-error: size > 1 or size == 1 but uniform is not an arry"

        -- create uniform names for identifies ending with [0]
        -- with expanding the indices
        expandArrayNames name size =
            let trimmed = take (length name - 3) name
            in map (\i -> trimmed ++ "[" ++ show i ++ "]") [0..(size-1)]

        isArrayName :: String -> Bool
        isArrayName = ("[0]" `isSuffixOf`)

--}
