module Yage.Rendering.Backend.Renderer.Logging where


import Yage.Prelude

import Control.Monad.Writer (tell)

import Yage.Rendering.Backend.Renderer.Types

logRenderM :: String -> Renderer ()
logRenderM msg = tell $ mempty{rlog'log = [msg]}

logCountObj :: Renderer ()
logCountObj = tell $ mempty{ rlog'objcount = 1 }

logCountTriangles :: Int -> Renderer ()
logCountTriangles tris = tell $ mempty{ rlog'tricount = tris }

logRenderMf :: String -> [String] -> Renderer ()
logRenderMf msg args = logRenderM $ format msg args

isEmptyRenderLog :: RenderLog -> Bool
isEmptyRenderLog = (mempty ==)