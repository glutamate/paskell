{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module Paskell.ToLLVM where

import Paskell.Expr
--import Text.Parsec.Expr
import Paskell.Syntax.Haskell
import Data.List
import Debug.Trace
import qualified LLVM.FFI.Core as LLVM
import qualified Foreign as FFI
import qualified Foreign.C.String as FFI
import Control.Monad.State.Strict
import Foreign.C.String


type C a = StateT Context IO a

data Context = Context {
    modRef :: !LLVM.ModuleRef
  , functionRef :: !LLVM.ValueRef
  , basicBlockRef :: !LLVM.BasicBlockRef
  , builderRef :: !LLVM.BuilderRef
  }


compile :: [D] -> IO LLVM.ModuleRef
compile ds = do
  modRef1 <- withCString "foo" $ LLVM.moduleCreateWithName 
  builderRef <- LLVM.createBuilder
  let initialContext = Context { modRef = modRef1
                               , functionRef = FFI.nullPtr
                               , basicBlockRef = FFI.nullPtr
                               , builderRef = builderRef
                               }
  c <- execStateT (compileMod ds) initialContext
  return (modRef c)

compileMod :: [D] -> C ()
compileMod ds = do 
  return ()

compileV :: V -> LLVM.ValueRef
compileV (VReal x) = LLVM.constReal LLVM.doubleType (realToFrac x)
--compileV 