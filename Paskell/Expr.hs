{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Paskell.Expr where

import Data.Generics
import Data.List
import Control.Monad
--import Control.Monad.State.Strict
import Data.IORef

data EvalS = ES {
                  values :: [(String, IORef V)],
                  decls :: [D]
                }

newtype EvalM a = EvalM { unEvalM :: EvalS -> IO (Either String (a, EvalS)) }

type FailIO a = IO (Either String a)

data D = DLet Pat E
       | DMkType String [String] [(String, [T])]
       | DDecTy [String] T
       | DImport String
         deriving (Show, Eq, Read)

data V = VReal Double
       | VInt Int
--       | VSeed Seed
       | VLam ([V]->FailIO V)
       | VString String
       | VCons String [V]
       | VRec [(String,V)]
       | VSig E
         deriving (Show, Eq, Read)

data T = TLam [T] T
       | TApp T T
       | TCon String
       | TVar String
       | TRec [(String,T)]
         deriving (Show, Eq, Read, Data, Typeable)

data E = ECon V 
       | EApp E [E]
       | EAssign Pat E
       | ELam [Pat] [E]
       | EVar String
       | ECase E [(Pat, E)]
       | ELet [(Pat,E)] E
       | ETy T E
         deriving (Show, Eq, Read)
       
data Pat = PLit V
         | PWild 
         | PVar String
         | PCons String [Pat]
         | PBang Pat
         | PTy T Pat
         | PWithRec
           deriving (Show, Eq, Read)

instance Show ([V]->FailIO V) where
   show f = "<function>"

instance Read ([V]->FailIO V) where
   readsPrec _ s = error $ "read: <function>" ++s

instance Eq ([V]->FailIO V) where
   f == g = error "eq: <function>"
