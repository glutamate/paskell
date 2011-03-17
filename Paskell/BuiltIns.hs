{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module Baysig.BuiltIns where

import Paskell.Expr
import Text.Parsec.Expr
import Paskell.Syntax.Haskell
import Data.List
import Debug.Trace

type BiF = (String, (T, V))

bifs :: [BiF]
bifs = 
  [ ("+", packF2 $ ((+)::Int->Int->Int)) ]