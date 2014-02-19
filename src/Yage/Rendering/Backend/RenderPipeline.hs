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


--instance Monad (RenderPipeline s m i) where
--    return v = RenderPass (undefined) (\_ -> return v)


--instance (Monad m, Functor m, Applicative m, WithSetup s m) => Applicative (RenderPipeline s m i) where
--    pure = PipeConst
    
--    (PipeConst f) <*> rx = fmap f rx  
--    rf <*> rx = PipeM $ \i -> runPipeline rf i <*> runPipeline rx i 


--instance (Monad m, WithSetup s m) => Category (RenderPipeline s m) where
--    id = PipeId
--    rb . ra = PipeM $ \i -> runPipeline rb =<< runPipeline ra i


--instance (Monad m, WithSetup s m) => Arrow (RenderPipeline s m) where
--    arr = PipeP

--    -- first :: RenderPass s m a b -> RenderPass s m (a, c) (b, c)
--    first ar = PipeM $ \(i, c) ->
--        liftM (, c) $ runPipeline ar i



runPipeline :: (Monad m, WithSetup s m) => RenderPipeline s m i o -> i -> m o
runPipeline (RenderPass s p)  = withSetup s . p


mkRenderPass :: setup -> (input -> m output) -> RenderPipeline setup m input output
mkRenderPass = RenderPass

