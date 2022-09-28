module Compiler where
import System.IO
import Types
import Exception

data Compiler = Compiler{
    compFileName :: String,
    compEnv :: Environment,
    compResult :: String,
    compError :: Error}

visit :: Compiler -> 


runCompiler :: Compiler -> IO ()
runCompiler inter = do 
  hSetBuffering stdout NoBuffering
  putStr "Enter the filename for compilation: " 
  input <- getLine
  putStr input
 
