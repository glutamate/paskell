{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleInstances, TypeSynonymInstances #-}

module Paskell.EvalM where

import Paskell.Expr 
import Control.Monad
import Data.IORef
import Control.Monad.State.Lazy
import Control.Monad.Error
import Control.Monad.Trans


{-instance Functor EvalM where
   fmap f (EvalM sm) = EvalM $ \evs -> do 
--         res1 <- sm evs
         case sm evs of
           Right iopair -> Right $ do
               (res, newevs) <- iopair
               return $ (f res, newevs)
           Left s -> Left s 

instance Monad EvalM where
   return x = EvalM $ \es -> Right $ return (x,es)
   EvalM sm >>= k = EvalM $ \evs -> do
--         res1 <- sm evs
         case sm evs of
           Right iopair -> Right $ do 
               (res, newevs) <- iopair
               unEvalM (k res) newevs
           Left s -> Left s
   fail s = failEvM s -}

{-instance MonadPlus (StateT EvalS (ErrorT String IO)) where
   mzero =  failEvM "mzero"
   mplus m1 m2 = m1 `catchError` (\e -> m2) --undefined
{-   mplus m1 m2 = EvalM $ \es -> do 
             res <- (unEvalM m1) es
             case res of 
               r@(Right _) -> return r
               Left _ -> (unEvalM m2) es -} -}

failEvM s = throwError s

withEState :: EvalS -> EvalM a -> FailIO a
withEState es ma = evalStateT ma es
 
withoutState :: FailIO a -> EvalM a
withoutState = lift {-mex = do
       ex <- lift mex
       case ex of
          Left s -> failEvM s
          Right x -> return x -}
                            
liftio = liftIO {-mio = EvalM $ \evs-> do
       x <- mio 
       return $ Right (x,evs)-}

{-get :: EvalM EvalS
get = EvalM $ \evs -> return $ Right (evs,evs)

put :: EvalS -> EvalM ()
put evs = EvalM $ \_ -> return $ Right ((), evs)-}


envToRefs :: [(String, V)] -> IO [(String, IORef V)] 
envToRefs exts = forM exts $ \(nm,v) -> do 
         ref <- (newIORef v )
         return (nm,ref)

--unrefEnv :: [(String, IORef V)] -> IO [(String, V)] 
unrefEnv exts = forM exts $ \(nm,v) -> do 
         ref <- readRef v
         return (nm,ref)

extendValues :: [(String, V)] -> EvalM ()
extendValues exts = do 
   ES vals tys <- get
   extRefs <- liftio $ envToRefs exts
   put $ ES (extRefs++vals) tys

extendValues1 :: [(String, V)] -> EvalM ()
extendValues1 exts = do 
   ES vals tys <- get
   extRefs <- liftio $ envToRefs exts
   put $ ES (vals++extRefs) tys

withExtensions :: [(String, V)] -> EvalM a -> EvalM a
withExtensions exts ma = do
   ES vals tys <- get
   extRefs <- liftio $  envToRefs exts
   put $ ES (extRefs++vals) tys
   x <- ma
   put $ ES (vals) tys
   return x

readRef ref = liftio $ readIORef ref
writeRef ref v = liftio $ writeIORef ref v

lookupVal :: String -> EvalM V
lookupVal nm =  lookupRef nm >>= readRef

lookupRef :: String -> EvalM (IORef V)
lookupRef nm = do
   ES vls _ <- get
   case lookup nm vls of
     Just ref -> return ref
     Nothing -> do
         env <- unrefEnv vls
         failEvM $ "lookUpVal: can't find "++nm ++" env=\n"++show env

modifyOrExtend :: [(String, V)] -> EvalM ()
modifyOrExtend = mapM_ mOE where
   mOE (nm,val) = 
      (lookupRef nm >>= (`writeRef` val)) `mplus` extendValues [(nm,val)]
