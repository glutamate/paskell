{-# LANGUAGE Rank2Types, ScopedTypeVariables, FlexibleInstances, OverloadedStrings, TypeSynonymInstances, TypeOperators, GADTs #-}
module Paskell.Syntax.Haskell where

import Paskell.Expr
import Paskell.EvalM
import Control.Monad
import Data.Maybe
import System.IO.Unsafe
import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.Trans

import Data.String

instance IsString E where
   fromString s = EVar s

instance IsString Pat where
   fromString s = PVar s

instance IsString [Pat] where
   fromString = map fromString . words 


infixl 0 =:
infixl 1 $-

f $- x = f x 

class HasAssign a where
   (=:) :: Pat -> E -> a

instance HasAssign D where
   p =: e = DLet p e

instance HasAssign E where
   p =: e = EAssign p e

tint = TCon "Int"
treal = TCon "Real"
tstring = TCon "String"
tunit = TCon "Unit"
a = TVar "a"
b = TVar "b"
c = TVar "c"
vunit = pack ()

infixr 3 ~>

targs ~> t2 = TLam targs t2

instance Num E where
   e1 + e2 = EVar "+" $> [e1, e2]
   e1 - e2 = EVar "-" $> [e1, e2]
   e1 * e2 = EVar "*" $> [e1, e2]
   abs e = EVar "abs" $> [e]
   signum e = EVar "signum" $> [e]
   fromInteger n = ECon (VInt $ fromInteger n)

instance Num Pat where
   e1 + e2 = error "Pattern nummeric instance"
   e1 - e2 = error "Pattern nummeric instance"
   e1 * e2 = error "Pattern nummeric instance"
   abs e = error "Pattern nummeric instance"
   signum e = error "Pattern nummeric instance"
   fromInteger n = PLit (VInt $ fromInteger n)


class Reify a where
    reify :: V-> Maybe a
    pack :: a->V
    packType :: a->T

instance Reify V where
    reify = Just
    pack = id
    packType = undefined

instance Reify Double where 
    reify (VReal x) = Just x
    reify v = Nothing
    pack = VReal
    packType _ = treal


instance Reify Int where 
    reify (VInt x) = Just x
    reify v = Nothing
    pack = VInt
    packType _ = tint

instance Reify () where 
    reify (VCons "unit" []) = Just ()
    reify v = Nothing
    pack () = VCons "unit" []
    packType _ =  TCon "Unit"

newtype String_ = String_ { unString :: [Char] }

instance Reify String_ where 
    reify (VString s) = Just $ String_ s
    reify v = Nothing
    pack (String_ s) = VString s
    packType _ = tint

instance Reify Bool where 
    reify (VCons "True" []) = Just True
    reify (VCons "False" []) = Just False
    reify v = Nothing
    pack True = VCons "True" []
    pack False = VCons "False" []
    packType _ = TCon "Bool"

instance (Reify a, Reify b) => Reify (a-> IO b) where
    reify (VLam f) 
          = Just $ 
             \hsArgs-> do
                res <- runErrorT $ f $ [pack hsArgs]
                
                case res of
                     Left s -> error $ "error in reify function: "++s
                     Right v -> return $ fromJust $ reify v 
    reify _ = Nothing
    pack f = VLam $
              \[v]-> case reify v of 
                    Nothing -> fail $ "pack function: fail for argument" 
                                      ++ show v++" for function of type "++show (packType f)
                    Just x ->  do v<- liftIO $ f x
                                  return $ pack v
    packType f = typeFIO f 

typeFIO :: forall a b. (Reify a, Reify b) => (a-> IO b) -> T
typeFIO f = let x = undefined :: a --undefined
                y = unsafePerformIO $ f x 
            in [packType x] ~> packType y

typeF :: forall a b. (Reify a, Reify b) => (a-> b) -> T
typeF f = let x = undefined :: a --undefined
              y = f x 
          in [packType x] ~> packType y

typeF2 :: forall a b c. (Reify a, Reify b, Reify c) => (a-> b -> c) -> T
typeF2 f = let x = undefined :: a --undefined
               y = undefined :: b
               z = f x y
          in [packType x, packType y] ~> packType y

packF1 :: (Reify a, Reify b) => (a->b) -> (T,V)
packF1 f = (typeF f, VLam $ \[v] -> case reify v of 
                    Nothing -> error $ "pack function: fail for argument" 
                                      ++ show v++" for function of type "++show (typeF f)
                    Just x ->  return $ pack $ (f x ))

packF2 :: (Reify a, Reify b, Reify c) => (a->b->c) -> (T,V)
packF2 f = (typeF2 f, VLam $ \[vx,vy] -> case liftM2 (,) (reify vx) (reify vy) of 
                    Nothing -> error $ "pack function: fail for argument" 
                                      ++ show vx++" for function of type "++show (typeF2 f)
                    Just (x,y) ->  return $ pack $ (f x y))



type TopLevel a = Writer [D] a
type CasePates = Writer [Pat :-> Function ()] ()
type Function a = Writer [E] a

infixl 1 -:>

p -:> es = tell [p :-> es]

instance HasAssign (TopLevel a) where
   p =: e = tell [DLet p e] >> return undefined

instance HasAssign (Function a) where
   p =: e = tell [p =: e] >> return undefined

lam :: [Pat] -> Function () -> E
lam pats fwriter = ELam pats $ execWriter fwriter

module_ :: TopLevel () -> [D]
module_ = execWriter

d :: D -> TopLevel ()
d = tell . (:[])

e :: ToExprs a => a -> Function ()
e = tell . toExprs

e $>> es = tell [e $> es]

for n m ix bd = "for" $>> [n, m, ELam ix (execWriter bd)]

infixl 5 :-> 


data a :-> b = a :-> b

data Then = Then
data Else = Else

if_ :: E -> Then ->Function () -> Else -> Function () -> Function ()
if_ p _ c _ a = tell [EIf p (execWriter c) (execWriter a)]

caseOf :: E -> CasePates -> Function ()
caseOf e pates = tell [ECase e $ map f $ execWriter pates]  where
   f (p:->es) = (p,execWriter es)

class ToExprs a where
   toExprs :: a -> [E]

instance ToExprs E where
   toExprs = (:[])

instance ToExprs [Char] where
   toExprs = (:[]) . EVar

instance ToExprs [E] where
   toExprs = id

instance ToExprs V where
   toExprs = (:[]) . ECon

instance ToExprs () where
   toExprs _ = [ECon (pack ())]

instance ToExprs (Function ()) where
   toExprs w = execWriter w


class CallResult a where
   ($>) :: E -> [E] -> a

instance CallResult (Function a) where
   f $> args = tell [EApp f args] >> return undefined

instance CallResult (E) where
   f $> args = EApp f args

--($>) = call

--($>) :: E -> [E] -> E
--f $> es = EApp f es
