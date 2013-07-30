module Yage.Resources where

import             Linear                          (V3(..), zero)
import             Linear.Quaternion               (Quaternion)
import             Graphics.Rendering.OpenGL       (GLfloat)


{--
-- | A 'YageResource' is loaded into a 'RenderEntity'
data YageResource = YageResource
    { resourceId         :: ResourceId
    , resourceDefinition :: ResourceDefinition
    }

type ResourceId = Int
data ResourceDefinition = YageResourceDefinition [ResourceDefinition]
                        | YageModelResource TriMesh
                        | YageShaderResource YageShader
                        | YageTextureResource
                        deriving (Show, Eq)

allShaders :: YageResource -> [ResourceDefinition]
allShaders (YageResource _ s@(YageShaderResource _))       = [s]
allShaders (YageResource _ (YageResourceDefinition defs))  = filter isShader defs
allShaders _ = []

allTextures :: YageResource -> [ResourceDefinition]
allTextures (YageResource _ YageTextureResource)           = [YageTextureResource]
allTextures (YageResource _ (YageResourceDefinition defs)) = filter (==YageTextureResource) defs
allTextures _ = []

allModels :: YageResource -> [ResourceDefinition]
allModels (YageResource _ m@(YageModelResource _))         = [m]
allModels (YageResource _ (YageResourceDefinition defs))   = filter isModel defs
allModels _ = []


isModel :: ResourceDefinition -> Bool
isModel (YageModelResource _) = True
isModel _ = False

isShader :: ResourceDefinition -> Bool
isShader (YageShaderResource _) = True
isShader _ = False

isTexture :: ResourceDefinition -> Bool
isTexture (YageTextureResource) = True
isTexture _ = False
--}
---------------------------------------------------------------------------------------------------

type Position = V3 GLfloat
type Orientation = Quaternion GLfloat
type Scale = V3 GLfloat

type Vertex = V3 GLfloat
type Index = Int

data TriMesh = TriMesh
    { meshId   :: !String
    , vertices :: ![Vertex]
    , indices  :: ![Index]
    , triCount :: !Int
    } deriving (Show)

instance Eq TriMesh where
    a == b = meshId a == meshId b

instance Ord TriMesh where
    compare a b = compare (meshId a) (meshId b)

mkTriMesh :: String -> [Vertex] -> [Index] -> TriMesh
-- some assertions for invalid meshes
mkTriMesh id vs ixs = TriMesh id vs ixs $ (length ixs) `quot` 3

--combine :: TriMesh -> TriMesh -> TriMesh
--combine a b =
--    TriMesh
--    { vertices  = vertices a ++ vertices b
--    , indices   = indices a  ++ map (+(length $ indices b)) (indices b)
--    , triCount  = triCount a + triCount b
--    }

---------------------------------------------------------------------------------------------------

data YageShaderResource = YageShaderResource
    { vert  :: FilePath
    , frag  :: FilePath
    } deriving (Show, Eq, Ord)

data RenderDefinition = RenderDefinition
    { defs :: (TriMesh, YageShaderResource)
    } deriving (Show, Eq, Ord)


