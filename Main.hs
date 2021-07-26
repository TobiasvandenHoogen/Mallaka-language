
import System.IO
import Data.Maybe
import Lexer 
import Parser 
import Data.Map
import Types
import Interpreter
----------Runner 

runResult :: Interpreter -> String -> Interpreter
runResult inter input = 
  newInter 
  where 
    lexer = Lexer {
      fileName = "Shell",
      inputText = input , 
      currentLine = 0, 
      currentPosition = Position{
                          index = 1,
                          column = 1,
                          line = 0}, 
      currentChar = head input, 
      tokenList = []}
    tokenLexer = createTokens lexer 
    parser = Parser {
      tokens = tokenList tokenLexer,
      tokenIndex = 1,
      currentToken = head (tokenList tokenLexer),
      currentNode = Nothing,
      file = fileName tokenLexer}
    nodeTree = parse parser 
    newInter = visit (fromJust (currentNode nodeTree)) inter

runInterpreter :: Interpreter -> IO ()
runInterpreter inter = do 
  hSetBuffering stdout NoBuffering
  putStr ">> " 
  input <- getLine

  let result = runResult inter input
  let printRes = if isNothing(currentResult result) then nullType definedTypes else show (numberValue (fromJust(currentResult result)))
  print( printRes )
  runInterpreter result

main :: IO ()
main = do
  let intptr = Interpreter {intprFileName = "Shell",
  intEnv = Environment{lookupTable = Data.Map.empty, 
  parent = Nothing},
  currentResult = Nothing}
  runInterpreter intptr
  main 
  return ()
