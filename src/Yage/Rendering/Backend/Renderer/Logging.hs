module Yage.Rendering.Backend.Renderer.Logging where


import Yage.Prelude


import Yage.Rendering.Backend.Renderer.Types
import Yage.Rendering.Backend.Renderer.Lenses

logRenderM :: String -> Renderer ()
logRenderM msg = scribe rlLog [msg]

logCountObj :: Renderer ()
logCountObj = scribe rlLogObjCount 1

logCountTriangles :: Int -> Renderer ()
logCountTriangles = scribe rlLogTriCount

logRenderMf :: String -> [String] -> Renderer ()
logRenderMf msg args = logRenderM $ format msg args

isEmptyRenderLog :: RenderLog -> Bool
isEmptyRenderLog = (mempty ==)