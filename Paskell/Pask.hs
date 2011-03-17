{-# LANGUAGE Rank2Types, ScopedTypeVariables, FlexibleInstances, OverloadedStrings #-}
module Main where

import Paskell.Expr
import Paskell.Eval
import Paskell.EvalM
import Paskell.Syntax.Haskell
import System.Environment
import Paskell.BuiltIns

runModule :: [D] -> IO EvalS
runModule ds = do
   let initS = ES [] []
   args <- getArgs
   res <- unEvalM (evalModule args (bifDs++ds)) initS 
   case res of
      Left s -> fail s
      Right (_, eS) -> return eS

evalModule :: [String] -> [D] -> EvalM ()
evalModule args ds = do
    mapM_ evalD ds
    ES vls _ <- get
    onHeadM [ mainRef | ("main", mainRef) <- vls] $ \mainRef -> do
           VLam vlam <- readRef mainRef
           _ <- liftio $ vlam $ map VString args
           return ()
    return ()

for :: D
for = "for" =: ELam ["counter", "f"] [ECase "counter" 
                                         [(0, ECon (pack ())),
                                          (1, "f" $> [])]
                                     ]


helloWorld :: [D]
helloWorld = 
  [ "x" =: 5,
    "main" =: (ELam ["s"] $ ["print" $> [ECon (VString "Hello World")],
                          "y" =: 11,
                          "y" =: 1+"y",
                          "print" $> ["showInt" $> ["y"]],
                          "print" $> ["s"] ])
  ]

main = do
     runModule helloWorld
--     mapM_ print helloWorld
     return ()

onHeadM :: Monad m => [a] -> (a -> m ()) -> m ()
onHeadM [] _ = return ()
onHeadM (x:_) f = f x