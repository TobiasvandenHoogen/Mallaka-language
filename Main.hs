
import System.IO
import Data.Maybe
import Lexer 
import Parser 
import Data.Map
import Types
import Compiler
import Exception 
----------Runner 

main :: IO ()
main = do
  let cmplr = Compiler {
              compFileName = "Shell",
              compEnv = Environment{lookupTable = Data.Map.empty},
              compResult = "", 
              compError = Error{hasOccurred = False, errorMessage = []}}
  runCompiler cmplr
  return ()
