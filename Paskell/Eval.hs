{-# LANGUAGE TypeOperators, DeriveDataTypeable, TypeSynonymInstances #-}

module Paskell.Eval where

import Paskell.Expr
import Paskell.EvalM
import Control.Monad
import Data.Maybe

evalD :: D -> EvalM ()
evalD (DLet p e) = do
   v <- evalE e
   exts <- matchPV p v
   extendValues exts
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
              allExts <- envToRefs $ concat 
                         $ catMaybes 
                         $ map (uncurry matchPV) 
                         $ zip ps vargs 

--              withExtensions (concat allExts)
              res <- (unEvalM (evalEs body)) $ ES (allExts++vals) ds
              case res of 
                Left s -> fail s
                Right (v,es) -> return $ Right v
evalE (ETy t e) = evalE e
evalE (ECase ex pats) = do 
  v <- evalE ex
  evalCase v pats

evalCase :: V -> [(Pat, E)] -> EvalM V
evalCase v [] = fail $ "evalCase: non-exhaustive case; no match for: "++show v      
evalCase v ((pat,e):rest) =     
    (do exts <- matchPV pat v 
        withExtensions exts $ evalE e
    ) `mplus` evalCase v rest 



evalEs :: [E] -> EvalM V
evalEs [] = fail "empty function body"
evalEs [e] = evalE e
evalEs (e:es) = evalE e >> evalEs es

matchPV :: MonadPlus m => Pat -> V -> m [(String,V)]
matchPV (PVar nm) v = return [(nm,v)]