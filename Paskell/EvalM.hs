{-# LANGUAGE TypeOperators, DeriveDataTypeable, TypeSynonymInstances #-}

module Paskell.EvalM where

import Paskell.Expr
import Control.Monad
import Data.IORef


instance Functor EvalM where
   fmap f (EvalM sm) = EvalM $ \evs -> do 
         res1 <- sm evs
         case res1 of
           Right (res, newevs) -> return $ Right (f res, newevs)
           Left s -> return $ Left s

instance Monad EvalM where
   return x = EvalM $ \es -> return $ Right (x,es)
   EvalM sm >>= k = EvalM $ \evs -> do
         res1 <- sm evs
         case res1 of
           Right (res, newevs) -> unEvalM (k res) newevs
   fail s = EvalM $ \_ -> return $ Left s

instance MonadPlus EvalM where
   mzero = fail "mzero"
   mplus m1 m2 = EvalM $ \es -> do 
             res <- (unEvalM m1) es
             case res of 
               r@(Right _) -> return r
               Left _ -> (unEvalM m2) es
                            

liftio mio = EvalM $ \evs-> do
       x <- mio 
       return $ Right (x,evs)

get :: EvalM EvalS
get = EvalM $ \evs -> return $ Right (evs,evs)

put :: EvalS -> EvalM ()
put evs = EvalM $ \_ -> return $ Right ((), evs)


extendValues :: [(String, V)] -> EvalM ()
extendValues exts = do 
   ES vals tys <- get
   extRefs <- forM exts $ \(nm,v) -> do 
         ref <- liftio (newIORef v )
         return (nm,ref)
   put $ ES (extRefs++vals) tys

withExtensions :: [(String, V)] -> EvalM a -> EvalM a
withExtensions exts = undefined

--failEv s = 

readRef ref = liftio $ readIORef ref
writeRef ref v = liftio $ writeIORef ref v

lookupVal :: String -> EvalM V
lookupVal nm =  lookupRef nm >>= readRef

lookupRef :: String -> EvalM (IORef V)
lookupRef nm = do
   ES vls _ <- get
   case lookup nm vls of
     Just ref -> return ref
     Nothing -> fail $ "lookUpVal: can't find "++nm

modifyOrExtend :: [(String, V)] -> EvalM ()
modifyOrExtend = mapM_ mOE where
   mOE (nm,val) = 
      (lookupRef nm >>= (`writeRef` val)) `mplus` extendValues [(nm,val)]
