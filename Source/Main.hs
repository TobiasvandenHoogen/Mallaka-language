import Data.Map
import Source.Interpreter
import Source.Exception 
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
