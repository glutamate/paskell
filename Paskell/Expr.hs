{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Paskell.Expr where

import Data.Generics
import Data.List
import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Error
import Data.IORef

data EvalS = ES {
                  values :: [(String, IORef V)],
                  decls :: [D]
                }

--newtype EvalM a = EvalM { unEvalM :: EvalS -> Either String (IO (a, EvalS)) }

type EvalM = StateT EvalS (ErrorT String IO)

type FailIO = ErrorT String IO

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
       | ECase E [(Pat, [E])]
       | ETy T E
       | EIf E [E] [E]
         deriving (Show, Eq, Read)
       
data Pat = PLit V
         | PWild 
         | PVar String
         | PCons String [Pat]
--         | PBang Pat
         | PTy T Pat
--         | PWithRec
           deriving (Show, Eq, Read)

instance Show ([V]->FailIO V) where
   show f = "<function>"

instance Read ([V]->FailIO V) where
   readsPrec _ s = error $ "read: <function>" ++s

instance Eq ([V]->FailIO V) where
   f == g = error "eq: <function>"

flatP e@(PVar nm) = [e]
flatP PWild = []
flatP (PTy _ p) = flatP p

qmap :: (Data a, Typeable b) => (b -> b) -> a -> a
qmap f = everywhere (mkT f)

--qcatMap :: (Data a, Typeable b) => (b -> [b]) -> a -> a
--qcatMap f = everywhere (mkT f)

qquery :: (Data a1, Typeable b) => (b -> [a]) -> a1 -> [a]
qquery qf = everything (++) ([] `mkQ` qf)

flat :: (Data a) => a -> [a]
flat = qquery (:[])
