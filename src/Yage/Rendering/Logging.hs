module Yage.Rendering.Logging where

import Yage.Prelude

import Control.Monad.Writer (tell)

import Yage.Rendering.Types

logRenderM :: String -> Renderer ()
logRenderM msg = tell $ mempty{rlog'log = [msg]}

logRenderMf :: String -> [String] -> Renderer ()
logRenderMf msg args = logRenderM $ format msg args

isEmptyRenderLog :: RenderLog -> Bool
isEmptyRenderLog = (mempty ==)
