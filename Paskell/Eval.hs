{-# LANGUAGE TypeOperators, DeriveDataTypeable, TypeSynonymInstances #-}

module Paskell.Eval where

import Paskell.Expr
import Paskell.EvalM
import Control.Monad

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
   vfun vargs
evalE (ELam ps body) = do
   return $ VLam $ \vargs -> do
              allExts <- mapM (uncurry matchPV) $ zip ps vargs
              withExtensions (concat allExts) $ evalEs body        
evalE (ETy t e) = evalE e

evalEs :: [E] -> EvalM V
evalEs [] = fail "empty function body"
evalEs [e] = evalE e
evalEs (e:es) = evalE e >> evalEs es

matchPV :: Pat -> V -> EvalM [(String,V)]
matchPV (PVar nm) v = return [(nm,v)]