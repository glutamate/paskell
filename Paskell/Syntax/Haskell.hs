{-# LANGUAGE Rank2Types, ScopedTypeVariables, FlexibleInstances #-}
module Paskell.Syntax.Haskell where

import Paskell.Expr
import Paskell.EvalM
import Control.Monad
import Data.Maybe
import System.IO.Unsafe

e1 $> eargs = EApp e1 eargs

tint = TCon "Int"
treal = TCon "Real"
a = TVar "a"
b = TVar "b"
c = TVar "c"

infixr 3 ~>

targs ~> t2 = TLam targs t2

instance Num E where
   e1 + e2 = EVar "+" $> [e1, e2]
   e1 - e2 = EVar "-" $> [e1, e2]
   e1 * e2 = EVar "*" $> [e1, e2]
   abs e = EVar "abs" $> [e]
   signum e = EVar "signum" $> [e]
   fromInteger n = ECon (VInt $ fromInteger n)


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
                let EvalM eslam = f $ [pack hsArgs]
                res::(Either String (V,EvalS)) <- eslam $ ES [] []
                case res of
                     Left s -> error $ "error in reify function: "++s
                     Right (v, newes) -> return $ fromJust $ reify v
    reify _ = Nothing
    pack f = VLam $
              \[v]-> case reify v of 
                    Nothing -> fail $ "pack function: fail for argument" 
                                      ++ show v++" for function of type "++show (packType f)
                    Just x ->  pack `fmap`  liftio (f x )
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

