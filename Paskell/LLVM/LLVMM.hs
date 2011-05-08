{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LLVM where

import qualified LLVM.FFI.Core as FFI
import qualified LLVMUtil
import Foreign.C.String (withCString, withCStringLen, CString, peekCString)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, newForeignPtr_, withForeignPtr)
import Foreign.C(CInt)
import Control.Monad.State
import Data.Maybe
import Debug.Trace

tr = trace

type Module = FFI.ModuleRef
type Function = FFI.ValueRef

data CGState = CGState {
      cg_module      :: LLVMUtil.Module,
      cg_vars        :: [(String, FFI.ValueRef)],
      cg_funs        :: [(String, Function)],
      cg_breakblocks :: [BasicBlock],
      cg_nextm       :: !Int,
      cg_builder     :: Builder,
      cg_function    :: Function,
      cg_nextf       :: !Int,
      cg_typedefs    :: [(String, FFI.TypeRef)],
      cg_typesizes   :: [(String, Int)],
      cg_structs     :: [(String, [(String, Int)])]
      }

cg0 m bld structs = CGState {
      cg_module      = m,
      cg_vars        = [],
      cg_funs        = [],
      cg_breakblocks = [],
      cg_nextm       = 1,
      cg_builder     = bld,
      cg_nextf       = 1,
      cg_typedefs    = [],
      cg_typesizes   = [],
      cg_structs     = structs
      }

getStructSize name = do
  structs <- gets cg_typesizes
  LLVM.lookup' name structs

type CodeGen a = StateT CGState IO a

type BasicBlock = FFI.BasicBlockRef

-- Transform Name data to string
{-k2llvmName :: Name -> String
k2llvmName (Name s t (Just _) a)
  | not (public a)            = s ++ '_':show t
k2llvmName (Name s t _ _)
  | t == 0 = s
k2llvmName (Name s t m a)       = id ++ tag ++ mod
  where
    id                          = if okForC s then s else "_sym"
    tag                         = if mod=="" || generated a || id=="_sym" then '_':show t else ""
    mod                         = maybe "" (('_' :) . modToundSc) m
k2llvmName (Prim CONS _)        = "CONS"
k2llvmName n                    = show n
 -}
-- debug lookup



-- type related
int       = FFI.int32Type
bit8      = FFI.int8Type
void      = FFI.voidType
ptr t     = FFI.pointerType t 0
word      = FFI.int32Type
poly      = ptr word
list      = ptr $ struct [ ptr word ]
cons      = ptr $ struct [ poly, ptr list ]
abstime   = struct [int, int]
-- xxx - problem
msg       = struct [ poly, abstime, abstime, ptr msg ]
-- xxx - end problem
struct ts = ptr $ LLVMUtil.structType ts False

{-getLLVMType :: AType -> CodeGen FFI.TypeRef
getLLVMType (TCon (Prim Int  _) _) = return int
getLLVMType (TCon (Prim LIST _) _) = return list
getLLVMType (TCon (Prim CONS _) _) = return $ ptr $ struct [ ptr word, poly, ptr list ]
getLLVMType (TCon (Prim WORD _) _) = return int
getLLVMType (TCon (Prim POLY _) _) = return $ ptr int
getLLVMType (TCon (Prim AbsTime _) _) = return abstime
getLLVMType (TCon (Prim Msg _) _) = return $ ptr msg
getLLVMType (TCon (Tuple 0 _) _) = return bit8
getLLVMType (TCon name _)          = do
  let name' = k2llvmName name
  typedefs <- gets cg_typedefs
  case lookup name' typedefs of
    Just t  -> return (ptr t)
    Nothing -> error $ "getLLVMType Error - " ++ name' ++ " - " ++ show typedefs
getLLVMType x = error $ "getLLVMType Error - " ++ show x

calcStructSize [] = return 0
calcStructSize (typ:rest) = do
  size <- getLLVMTypeSize typ
  rest <- calcStructSize rest
  return $ size + rest

words :: Int -> Int
words bytes = Prelude.div (bytes+4-1) 4

getLLVMTypeSize :: AType -> CodeGen Int
getLLVMTypeSize (TCon (Prim Int  _) _) = return 4
getLLVMTypeSize (TCon (Prim WORD _) _) = return 4
getLLVMTypeSize (TCon (Prim POLY _) _) = return 4
getLLVMTypeSize (TCon _ _) = return 4
-}


addParam (name, p) = do
  typ <- liftIO $ FFI.typeOf p
  r1 <- alloca typ
  store p r1
  addVar name r1
  return ()


{-isLocalStruct :: AType -> Bool
isLocalStruct (TCon (Prim _ _) _) = False
isLocalStruct _                   = True

addStruct :: Name -> Decl -> CodeGen ()
addStruct sname (Struct _ fields _) = do
  let sname'  = k2llvmName sname
  let fields' = map snd fields
  fields'' <- mapM fixfields fields'
  let typ = LLVMUtil.structType fields'' False
  size <- calcStructSize [ t | (ValT t) <- fields' ]
  modify (\s -> s { cg_typesizes = (sname', (LLVM.words size)) : cg_typesizes s})
  modify (\s -> s { cg_typedefs = (sname', typ) : cg_typedefs s})
  typedefs <- gets cg_typedefs
  mod <- gets cg_module
  liftIO $ withCString sname' $ \cstring -> FFI.addTypeName (LLVMUtil.fromModule mod) cstring (ptr typ)
  return ()
    where
      fixfields (ValT timbertype) = do
        llvmtype <- getLLVMType timbertype
        return llvmtype
      fixfields (FunT _ params rettyp) = do
        rettyp' <- getLLVMType rettyp
        params' <- mapM getLLVMType params
        return $ LLVMUtil.functionType False rettyp' params'
-}
genFSym :: CodeGen String
genFSym = do
    s <- get
    let n = cg_nextf s
    put (s { cg_nextf = n + 1 })
    return $ "_L" ++ show n

addBreakBlock :: BasicBlock -> CodeGen ()
addBreakBlock bb = do
  modify (\s -> s { cg_breakblocks = bb : cg_breakblocks s } )

dropBreakBlock :: CodeGen ()
dropBreakBlock = do
  modify (\s -> s { cg_breakblocks = tail (cg_breakblocks s) })

getBreakBlock :: CodeGen BasicBlock
getBreakBlock = do
  bb <- gets cg_breakblocks
  return (head bb)

lookup' m n = do
  tr $ "lookup' : " ++ show m ++ " in: " ++ show n
  return $ fromJust $ lookup m n

lookup2 :: String -> [(String,a)] -> CodeGen a
lookup2 nm vars  = return $ fromJust $ lookup nm vars

lookupVars :: String -> CodeGen FFI.ValueRef
lookupVars name = do
  vars <- gets cg_vars
  lookup2 name vars

lookupFuns :: String -> CodeGen Function
lookupFuns name = do
  funs <- gets cg_funs
  lookup2 name funs

setFunction :: Function -> CodeGen ()
setFunction f = do
  s <- get
  put (s {cg_function = f})

addFunction :: String -> Function -> CodeGen ()
addFunction name ref = do
  s <- get
  put (s {cg_funs = (name, ref) : cg_funs s})


setVars :: [(String, FFI.ValueRef)] -> CodeGen ()
setVars vars = do
  s <- get
  put (s {cg_vars = vars})

addVar :: String -> FFI.ValueRef -> CodeGen ()
addVar name ref = do
  s <- get
  put (s {cg_vars = (name, ref) : cg_vars s})

lookupVar :: String -> CodeGen (Maybe FFI.ValueRef)
lookupVar name = do
  vars <- gets cg_vars
  --tr $ "lookupVar : " ++ name ++ " in: " ++ show vars
  case (lookup name vars) of
    Nothing -> return Nothing
    Just reg -> return (Just reg)

newBasicBlock :: CodeGen BasicBlock
newBasicBlock = genFSym >>= newNamedBasicBlock

newNamedBasicBlock :: String -> CodeGen BasicBlock
newNamedBasicBlock name = do
  f <- gets cg_function
  liftIO $ withCString name $ \nameptr -> FFI.appendBasicBlock f nameptr

defineBasicBlock bb = do
    bld <- gets cg_builder
    liftIO $ withBuilder bld $ \ bldptr ->
      FFI.positionAtEnd bldptr bb

-- use LLVMUtil instead
createBuilder :: IO Builder
createBuilder = do
    ptr <- FFI.createBuilder
    liftM Builder $ newForeignPtr FFI.ptrDisposeBuilder ptr

newtype Builder = Builder {
  fromBuilder :: ForeignPtr FFI.Builder
  } deriving (Show)

withBuilder :: Builder -> (FFI.BuilderRef -> IO a) -> IO a
withBuilder = withForeignPtr . fromBuilder

withCurrentBuilder body = do
    bld <- gets cg_builder
    liftIO $ withBuilder bld body

withCurrentBuilder_ :: (FFI.BuilderRef -> IO a) -> CodeGen ()
withCurrentBuilder_ body = do
  withCurrentBuilder body >> return ()

-- instructions

unreachable = withCurrentBuilder_ $ \bldref -> do
  FFI.buildUnreachable bldref

ret e1 = withCurrentBuilder $ \bldref -> do
  FFI.buildRet bldref e1
  return ()

iadd e1 e2 = withCurrentBuilder $ \bldref -> do
               withCString "" $ \cString -> FFI.buildAdd bldref e1 e2 cString
isub e1 e2 = withCurrentBuilder $ \bldref -> do
               withCString "" $ \cString -> FFI.buildSub bldref e1 e2 cString
br bb = withCurrentBuilder $ \bldref -> FFI.buildBr bldref bb >> return ()

call name args = do
  fun <- lookupFuns name
  withCurrentBuilder $ \bldref -> LLVMUtil.makeCall fun bldref args

condbr e bb1 bb2 = withCurrentBuilder $ \bldref -> FFI.buildCondBr bldref e bb1 bb2

switch val dflt arms = do
    withCurrentBuilder $ \ bldPtr -> do
        inst <- FFI.buildSwitch bldPtr val dflt (fromIntegral $ length arms)
        sequence_ [ FFI.addCase inst v bb | (v, bb) <- arms ]

icmp p e1 e2 = withCurrentBuilder $ \bldref -> do
  withCString "" $ \cString -> FFI.buildICmp bldref (fromIntPredicate p) e1 e2 cString

ptrtoint typ e1 = withCurrentBuilder $ \bldref -> do
  withCString "" $ \cString -> FFI.buildPtrToInt bldref e1 typ cString

bitcast typ e1 = withCurrentBuilder $ \bldref -> do
  withCString "" $ \cString -> FFI.buildBitCast bldref e1 typ cString

store e1 e2 = withCurrentBuilder $ \bldref -> do
  FFI.buildStore bldref e1 e2
  return ()

load e1 = withCurrentBuilder $ \bldref -> do
  withCString "" $ \cString -> FFI.buildLoad bldref e1 cString

alloca typ = withCurrentBuilder $ \bldref -> do
  withCString "" $ \cString -> FFI.buildAlloca bldref typ cString

getStructField :: FFI.ValueRef -> Int -> CodeGen FFI.ValueRef
getStructField struct ix = do
  withCurrentBuilder $ \ bldref ->
    LLVMUtil.withArrayLen [constInt 0, constInt ix] $ \numIndices indices ->
    withCString "" $ \ cString ->
    FFI.buildGEP bldref struct indices (fromIntegral numIndices) cString

constInt n = FFI.constInt (FFI.integerType 32) (fromIntegral n) (fromIntegral 1)

getStructIndex sname field = do
  structs <- gets cg_structs
  tr $ "lookup " ++ sname ++ " in: " ++ show structs
  let struct = fromJust $ lookup sname structs
  tr $ "lookup " ++ field ++ " in: " ++ show struct
  let idx = fromJust $ lookup field struct
  return idx

-- stolen
data IntPredicate =
    IntEQ                       -- ^ equal
  | IntNE                       -- ^ not equal
  | IntUGT                      -- ^ unsigned greater than
  | IntUGE                      -- ^ unsigned greater or equal
  | IntULT                      -- ^ unsigned less than
  | IntULE                      -- ^ unsigned less or equal
  | IntSGT                      -- ^ signed greater than
  | IntSGE                      -- ^ signed greater or equal
  | IntSLT                      -- ^ signed less than
  | IntSLE                      -- ^ signed less or equal
    deriving (Eq, Ord, Enum, Show)

fromIntPredicate :: IntPredicate -> CInt
fromIntPredicate p = fromIntegral (fromEnum p + 32)
