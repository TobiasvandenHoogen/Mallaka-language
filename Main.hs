
import System.IO
import Data.Maybe
import Lexer 
import Parser 
import Types


----------Runner 

main :: IO ()
main = do 
  hSetBuffering stdout NoBuffering
  putStr ">> " 
  input <- getLine
  let lexer = Lexer {
    fileName = "Shell",
    inputText = input , 
    currentLine = 0, 
    currentPosition = 
      Position{index = 1,
        column = 1,
        line = 0}, 
    currentChar = head input, 
    tokenList = []}

  let tok = createTokens lexer 

  let parser = Parser{
  tokens = tokenList tok,
  tokenIndex = 1,
  currentToken = head (tokenList tok),
  currentNode = Nothing,
  file = fileName tok 
  }

  let newParser = parse parser 

  print(currentNode newParser)
  
  main 
  return ()
