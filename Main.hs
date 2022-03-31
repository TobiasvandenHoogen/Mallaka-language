
import System.IO
import Data.Maybe
import Lexer 
import Parser 
import Data.Map
import Types
import Interpreter
import Exception 
----------Runner 

main :: IO ()
main = do
  let intptr = Interpreter {intprFileName = "Shell",
  intEnv = Environment{lookupTable = Data.Map.empty, 
  parent = Nothing},
  currentResult = Nothing,
  printResultList = [],
  intError = Error{hasOccurred = False, errorMessage = []}}
  runInterpreter intptr
  return ()
