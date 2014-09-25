{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Yage.Prelude as P
import Test.Hspec

import Data.ByteString.Char8 as BS
import Data.Map.Strict as M
import Data.Set        as S
import Data.Foldable ( traverse_ )

import Yage.Rendering.Shader as Shader
import Graphics.UI.GLFW as GLFW
import qualified Yage.Core.OpenGL as GL


hints :: [WindowHint]
hints =
    [ WindowHint'ContextVersionMajor  4
    , WindowHint'ContextVersionMinor  1
    , WindowHint'OpenGLProfile        OpenGLProfile'Core
    , WindowHint'ClientAPI            ClientAPI'OpenGL
    , WindowHint'OpenGLForwardCompat  True
    , WindowHint'Visible              False
    , WindowHint'OpenGLDebugContext   True
    ]

withOpenGLContext :: IO () -> IO ()
withOpenGLContext action =
    bracket -- init
            ( do
                (\b -> print ("init: " ++ show b)) =<< GLFW.init
                v <- getVersion
                print $ "GLFW: " ++ show v
                mapM_ GLFW.windowHint hints
                mwin <- createWindow 1 1 "ShaderLocationTest" Nothing Nothing
                print $ "window: " ++ show mwin
                GLFW.makeContextCurrent mwin
                return mwin
            )
            -- term
            (\mwin -> do
                traverse_ GLFW.destroyWindow mwin
                GLFW.terminate
            )
            -- exec
            (const action)


main :: IO ()
main = hspec $ around withOpenGLContext $ do

    describe "glsl shader locations" $ do
        it "has correct and complete attrib & uniform locations (even for arrays)" $ do
            prog <- Shader.loadShaderProgram shaderSrcs

            GL.attribs prog
                `shouldBe` M.fromList [("position", (GL.AttribLocation 0, GL.FloatVec2))]

            M.keysSet (GL.uniforms prog)
                `shouldBe` S.fromList [ "fade_factor", "singleData.size"

                                      , "singleData.uv[0]", "singleData.uv[1]"
                                      , "singleData.uv[2]", "singleData.uv[3]"

                                      , "textureData[0].size", "textureData[1].size"

                                      , "textureData[0].uv[0]", "textureData[0].uv[1]"
                                      , "textureData[0].uv[2]", "textureData[0].uv[3]"

                                      , "textureData[1].uv[0]", "textureData[1].uv[1]"
                                      , "textureData[1].uv[2]", "textureData[1].uv[3]"

                                      , "textures[0]", "textures[1]"
                                      ]


-------------------------------------------------------------------------------
shaderSrcs :: [(ShaderType, ByteString)]
shaderSrcs =
    [ ( VertexShader  , BS.pack vertexShader )
    , ( FragmentShader, BS.pack fragmentShader )
    ]
-- | Vertex Shader for tests
--
vertexShader :: String
vertexShader = [qStr|
#version 410

in vec2 position;
out vec2 texcoord;

void main()
{
    gl_Position = vec4(position, 0.0, 1.0);
    texcoord = position * vec2(0.5) + vec2(0.5);
}

|]

-- | Fragment Shader for tests
--
fragmentShader :: String
fragmentShader = [qStr|
#version 410

uniform float fade_factor;

// interface blocks not supported
/*
uniform ABlock
{
    float some_var;
    vec3 another_var[4];
} blocks[4];
*/

struct Tex
{
    vec2 uv[4];
    vec2 size;
};

uniform Tex singleData;
uniform Tex textureData[2];
uniform sampler2D textures[2];
in vec2 texcoord;

layout (location = 0) out vec4 pixelColor;

void main()
{
    textureData[0].uv;
    textureData[1].uv;
    singleData;
    // blocks[0].some_var; // interface blocks not supported
    pixelColor = mix(
        texture(textures[0], texcoord),
        texture(textures[1], texcoord),
        fade_factor);
}
|]
