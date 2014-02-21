{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
module Yage.Rendering.Backend.RenderPipeline where

import Yage.Prelude

class WithSetup s m where
    withSetup :: s -> m a -> m a



data RenderPipeline setup m input output where
    RenderPass :: { setup  :: setup
                  , pass   :: input -> m output -- the target is just for references, like 'this'
                  } -> RenderPipeline setup m input output


instance (Monad m, Functor m, WithSetup s m) => Functor (RenderPipeline s m i) where
    fmap f rp@(RenderPass s _)    = RenderPass s (\i -> fmap f (runPipeline rp i))


runPipeline :: (Monad m, WithSetup s m) => RenderPipeline s m i o -> i -> m o
runPipeline (RenderPass s p)  = withSetup s . p


mkRenderPass :: setup -> (input -> m output) -> RenderPipeline setup m input output
mkRenderPass = RenderPass

