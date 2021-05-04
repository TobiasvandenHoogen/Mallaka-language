module Interpreter where

type Ident = String 

type Env = [(Ident, Value)]

data Expr = Integer Int
        | Plus Expr Expr
        | Minus Expr Expr 
        | Multiply Expr Expr 
        | Divide Expr Expr
        | Modulo Expr Expr 
        | Var Ident 
        | Assign Ident Expr Expr 

data Value = IntegerVal Int 

eval :: Expr -> Env -> Value
eval (Integer i) env = IntegerVal i
eval (Plus expr1 expr2) env = let (IntegerVal val1) = eval expr1 env in 
                              let (IntegerVal val2) = eval expr2 env in 
                                    IntegerVal(val1 + val2)  
eval (Minus expr1 expr2) env = let (IntegerVal val1) = eval expr1 env in 
                               let (IntegerVal val2) = eval expr2 env in 
                                    IntegerVal(val1 - val2)  
eval (Multiply expr1 expr2) env = let (IntegerVal val1) = eval expr1 env in 
                                let (IntegerVal val2) = eval expr2 env  in 
                                    IntegerVal(val1 * val2)  
eval (Divide expr1 expr2) env = let (IntegerVal val1) = eval expr1 env in 
                                let (IntegerVal val2) = eval expr2 env in 
                                    IntegerVal(val1 `div` val2)  
eval (Modulo expr1 expr2) env = let (IntegerVal val1) = eval expr1 env in 
                                let (IntegerVal val2) = eval expr2 env in 
                                    IntegerVal(val1 `mod` val2)  
eval (Var i) env = find env i
eval (Assign i expr1 expr2) env = eval expr2 (elab env i expr1)

find env i = snd $ head $ filter(\(i', _) -> i == i') env

elab env i e = (i, eval e env):env 

main :: IO ()
main = putStrLn "Hello world!"