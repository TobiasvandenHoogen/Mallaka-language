
import System.IO
import Data.Maybe
import Lexer 
import Parser 
import Data.Map
import Types
import Interpreter
----------Runner 

main :: IO ()
main = do
  let intptr = Interpreter {intprFileName = "Shell",
  intEnv = Environment{lookupTable = Data.Map.empty, 
  parent = Nothing},
  currentResult = Nothing}
  runInterpreter intptr
  return ()
