{-# LANGUAGE Rank2Types, ScopedTypeVariables, FlexibleInstances, OverloadedStrings #-}
module Main where

import Paskell.Expr
import Paskell.Eval
import Paskell.EvalM
import Paskell.Syntax.Haskell
import System.Environment
import Paskell.BuiltIns
import Control.Monad.State.Lazy
import Control.Monad.Error


runModule :: [D] -> IO EvalS
runModule ds = do
   let initS = ES [] []
   args <- getArgs
   res <- runErrorT $ execStateT (evalModule args (bifDs++ds)) initS 
   case res of
      Left s -> fail s
      Right eS -> return eS

evalModule :: [String] -> [D] -> EvalM ()
evalModule args ds = do
    mapM_ evalD ds
    ES vls _ <- get
    onHeadM [ mainRef | ("main", mainRef) <- vls] $ \mainRef -> do
           VLam vlam <- readRef mainRef
           _ <- lift $ vlam $ map VString args
           return ()
    return ()

unite = ECon (pack ())

estr = ECon . VString

forDecl :: D
forDecl = "for" =: ELam "counter f" [ECase "counter" 
                                         [(0, [unite] ),
                                          (1, ["f" $> ["counter"]]),
                                          ("n", ["f" $> ["counter"], 
                                                 "for" $> [("counter"-1), "f"]])]
                                     ]

myProcD :: D
myProcD = "myProc" =: ELam "t" ["print" $> [estr "hello from myproc"],
                                "print" $> ["showInt" $> ["t"]]]


helloWorld :: [D]
helloWorld = 
  [ "x" =: 5,
    forDecl,
    myProcD,
    "main" =: (ELam "s" $ ["print" $> [estr "Hello World"],
                          "y" =: 9,
                          "y" =: 1+"y",
                          "z" =: 1,
                          "myProc" $> [5],
                          "print" $> ["showInt" $> ["z"]],
                          "print" $> ["s"],
                          "for"   $> [10, ELam "i" [
                                 "z" =: "z"+"i",
                                 "print" $> [estr "inside for!"]
                              ]],
                          "print" $> ["showInt" $> ["z"]],
                          "print" $> [estr "goodbye!"]
               ])

  ]

main = do
     mapM_ print helloWorld
     runModule helloWorld
--     mapM_ print helloWorld
     return ()

onHeadM :: Monad m => [a] -> (a -> m ()) -> m ()
onHeadM [] _ = return ()
onHeadM (x:_) f = f x