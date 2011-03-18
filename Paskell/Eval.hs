{-# LANGUAGE TypeOperators, DeriveDataTypeable, TypeSynonymInstances #-}

module Paskell.Eval where

import Paskell.Expr
import Paskell.EvalM
import Control.Monad
import Control.Monad.State.Lazy
import Data.IORef

import Data.Maybe

evalD :: D -> EvalM ()
evalD (DLet p e) = do
   ES env ds <- get
   refs <- mapM (\nm -> do ref <- liftIO $ newIORef (VCons "unit" [])
                           return (nm,ref)) $ [nm | PVar nm <- flatP p]
   put $ ES (refs++env) ds
   v <- evalE e
   exts <- matchPV p v
   modifyOrExtend exts
evalD (DDecTy _ _) = return ()
evalD (DMkType _ _ _) = return ()
evalD (DImport _) = return ()

evalE :: E -> EvalM V
evalE (ECon v) = return v
evalE (EVar nm) = do
   lookupVal nm
evalE (EAssign p e) = do
      v <- evalE e
      exts <- matchPV p v
      modifyOrExtend exts
      return v
evalE (EApp efun eargs) = do
   VLam vfun <- evalE efun
   vargs <- mapM evalE eargs
   withoutState $ vfun vargs
evalE (ELam ps body) = do
   ES vals ds <- get
   return $ VLam $ \vargs -> do
              allExts <- liftIO $ envToRefs $ concat 
                         $ catMaybes 
                         $ map (uncurry matchPV) 
                         $ zip ps vargs 

--              withExtensions (concat allExts)
              withEState (ES (allExts++vals) ds) $ evalEs body
              {-case res of 
                Left s -> failEvM s
                Right (v,es) -> return v -}
evalE (ETy t e) = evalE e
evalE (ECase ex pats) = do 
  v <- evalE ex
  evalCase v pats

evalCase :: V -> [(Pat, [E])] -> EvalM V
evalCase v [] = failEvM $ "evalCase: non-exhaustive case; no match for: "++show v      
evalCase v ((pat,e):rest) =     
    case matchPV pat v of
        Just exts -> withExtensions exts $ evalEs e
        Nothing -> evalCase v rest 



evalEs :: [E] -> EvalM V
evalEs [] = failEvM "empty function body"
evalEs [e] = evalE e
evalEs (e:es) = evalE e >> evalEs es

matchPV :: MonadPlus m => Pat -> V -> m [(String,V)]
matchPV (PVar nm) v = return [(nm,v)]
matchPV PWild v = return []
--matchPV (PBang p) v = matchPV p v
matchPV (PTy t p) v = matchPV p v
matchPV p@(PLit v1) v2 | v1 == v2 = return []
                       | otherwise = mzero {-error $"pat "++show p++
                                           "with : "++show v2 -}
matchPV (PCons cnm1 pats) (VCons cnm2 vls) 
     | cnm1 == cnm2 = matchCons $ zip pats vls
     | otherwise = mzero
matchPV p@(PCons _ _) v = mzero -- error $"pat "++show p++"with : "++show v

matchCons :: MonadPlus m => [(Pat, V)] -> m [(String, V)]
matchCons [] = return []
matchCons ((pat, v):patvs) = do
  env1 <- matchPV pat v
  env <- matchCons patvs
  return $ env1++env
