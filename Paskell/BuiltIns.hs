{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module Paskell.BuiltIns where

import Paskell.Expr
--import Text.Parsec.Expr
import Paskell.Syntax.Haskell
import Data.List
import Debug.Trace

type BiF = (String, (T, V))


bifs :: [BiF]
bifs = 
  [ ("+", packF2 $ ((+)::Int->Int->Int)),
    ("-", packF2 $ ((-)::Int->Int->Int)),
    ("*", packF2 $ ((*)::Int->Int->Int)),
    ("print", ([tstring] ~> tunit, pack $ putStrLn . unString)),
    ("showInt", packF1 $ ((String_ . show)::Int->String_)) ]

bifsToDs :: [BiF] -> [D]
bifsToDs = concatMap f where
   f (nm, (t,v)) = [DDecTy [nm] t, DLet (PVar nm) (ECon v)]

bifDs = bifsToDs bifs