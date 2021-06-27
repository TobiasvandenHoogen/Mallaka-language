module Lexer where 

import Data.Char
import Exception 
import Types 



data Lexer = Lexer{ 
    fileName :: String,
    inputText :: String,
    currentLine :: Int,
    currentPosition :: Position,
    currentChar :: Char,
    tokenList :: [Token]}

advancePosition :: Position -> Char -> Position
advancePosition pos currentChar =
    let checkColumn = if currentChar == '\n' then 0 else column pos in 
    let checkLine = if currentChar == '\n' then line pos + 1  else line pos in 
    Position{ index = index pos + 1,
              column = checkColumn,
              line = checkLine}

advanceLexer :: Lexer -> Lexer 
advanceLexer lexer = 
    let char = 
            if index (currentPosition lexer) < length (inputText lexer) 
            then take (index (currentPosition lexer)) (drop ( index (currentPosition lexer)) (inputText lexer)  ) 
            else "\00"
        result = Lexer {fileName = fileName lexer,
                        inputText = inputText lexer, 
                        currentPosition = advancePosition (currentPosition lexer) (currentChar lexer), 
                        currentLine = currentLine lexer,
                        currentChar =  head char,
                        tokenList = tokenList lexer}
    in result  


createTokens :: Lexer -> Lexer
createTokens lexer = 
  tokenizer where 

    tokenizer = 
      if currentChar lexer == '\00'
      then addToken (lexer) Token {tokenType = endOfFile definedTypes, val = Nothing , pos = currentPosition lexer}
      else tokens where 
        tokens =  
          createTokens newLexer where 
            newLexer
              | isDigit (currentChar lexer) = makeNumber lexer 
              | currentChar lexer == ' ' = advanceLexer lexer 
              | currentChar lexer == '+' = addToken (advanceLexer lexer) Token {tokenType = plusOperation definedTypes, val = Nothing, pos = currentPosition lexer}
              | currentChar lexer == '-' = addToken (advanceLexer lexer) Token {tokenType = minusOperation definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == '*' = addToken (advanceLexer lexer) Token {tokenType = multiplyOperation definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == '/' = addToken (advanceLexer lexer) Token {tokenType = divisionOperation definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == '(' = addToken (advanceLexer lexer) Token {tokenType = leftParent definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == ')' = addToken (advanceLexer lexer) Token {tokenType = rightParent definedTypes, val = Nothing, pos = currentPosition lexer }
              | otherwise = throwError(InvalidCharError (fileName lexer) ( "\"" ++ [currentChar lexer] ++ "\"") (currentPosition lexer))


addToken :: Lexer -> Token -> Lexer
addToken lexer token = 
  lexer {tokenList = tokenList lexer ++ [token]}


makeNumber :: Lexer -> Lexer
makeNumber lexer = 
  makeNumbers lexer 0 "" where 
    makeNumbers :: Lexer -> Int -> String -> Lexer
    makeNumbers lexer dotCount numberString 
      | (currentChar lexer == '.') && (dotCount == 0) = 
        makeNumbers (advanceLexer lexer) (dotCount + 1) (numberString ++ [currentChar lexer])
      | isDigit(currentChar lexer) = 
        makeNumbers (advanceLexer lexer) dotCount (numberString ++ [currentChar lexer])
      | otherwise = 
        if dotCount == 0
        then
          addToken lexer Token{tokenType = intType definedTypes, val = Just(Int(read numberString :: Int)), pos = currentPosition lexer}
        else
          addToken lexer Token{tokenType = floatType definedTypes, val = Just(Float(read numberString :: Float)), pos = currentPosition lexer}
