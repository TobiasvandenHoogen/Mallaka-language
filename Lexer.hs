module Lexer where 

import Data.Char
import Data.Maybe
import Exception 
import Types 


data Lexer = Lexer{ 
    fileName :: String,
    inputText :: String,
    currentLine :: Int,
    currentPosition :: Position,
    currentChar :: Char,
    lexerError :: Error,
    tokenList :: [Token]
    } deriving (Show)

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
                        lexerError = lexerError lexer,
                        tokenList = tokenList lexer}
    in result  


createTokens :: Lexer -> Lexer
createTokens lexer 
  | hasOccurred (lexerError lexer) = lexer
  | otherwise = tokenizer where 

    tokenizer = 
      if currentChar lexer == '\00'
      then addToken (lexer) Token {tokenType = endOfFile definedTypes, val = Nothing , pos = currentPosition lexer}
      else tokens where 
        tokens =  
          createTokens newLexer where 
            newLexer
              | isDigit (currentChar lexer) = makeNumber lexer 
              | isAlpha (currentChar lexer) = makeWord lexer 
              | currentChar lexer == ' ' = advanceLexer lexer 
              | currentChar lexer == '\n' = advanceLexer lexer 
              | currentChar lexer == '+' = addToken (advanceLexer lexer) Token {tokenType = plusOperation definedTypes, val = Nothing, pos = currentPosition lexer}
              | currentChar lexer == '-' = 
                case currentChar checkNextChar of 
                  '>' ->  addToken (advanceLexer checkNextChar) Token {tokenType = openStatement definedTypes, val = Nothing, pos = currentPosition lexer }
                  _  ->   addToken (advanceLexer lexer) Token {tokenType = minusOperation definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == '*' = addToken (advanceLexer lexer) Token {tokenType = multiplyOperation definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == '/' = addToken (advanceLexer lexer) Token {tokenType = divisionOperation definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == '%' = addToken (advanceLexer lexer) Token {tokenType = modOperation definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == '^' = addToken (advanceLexer lexer) Token {tokenType = powerOperation definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == '$' = addToken (advanceLexer lexer) Token {tokenType = indexOperation definedTypes, val = Nothing, pos = currentPosition lexer}
              | currentChar lexer == '=' =
                if currentChar checkNextChar == '=' 
                then addToken (advanceLexer checkNextChar) Token {tokenType = equalOperation definedTypes, val = Nothing, pos = currentPosition lexer }
                else addToken (advanceLexer lexer) Token {tokenType = assignOperation definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == '(' && currentChar checkNextChar == ')' = addToken (advanceLexer checkNextChar) Token {tokenType = nullType definedTypes, val = Nothing, pos = currentPosition lexer}
              | currentChar lexer == '(' = addToken (advanceLexer lexer) Token {tokenType = leftParent definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == ')' = addToken (advanceLexer lexer) Token {tokenType = rightParent definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == '[' = addToken (advanceLexer lexer) Token {tokenType = openList definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == ']' = addToken (advanceLexer lexer) Token {tokenType = closeList definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == '\"' = makeString lexer
              | currentChar lexer == '{' = addToken (advanceLexer lexer) Token {tokenType = openParameter definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == '}' = addToken (advanceLexer lexer) Token {tokenType = closeParameter definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == ',' = addToken (advanceLexer lexer) Token {tokenType = seperatorParameter definedTypes, val = Nothing, pos = currentPosition lexer}
              | currentChar lexer == '&' = addToken (advanceLexer lexer) Token {tokenType = andOperation definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == '|' = addToken (advanceLexer lexer) Token {tokenType = orOperation definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == ';' = addToken (advanceLexer lexer) Token {tokenType = statementSeperator definedTypes, val = Nothing, pos = currentPosition lexer}
              | currentChar lexer == '!' = 
                if currentChar checkNextChar == '=' 
                then addToken (advanceLexer checkNextChar) Token {tokenType = notEqualOperation definedTypes, val = Nothing, pos = currentPosition lexer }
                else addToken (advanceLexer lexer) Token {tokenType = notOperation definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == '<' =
                case currentChar checkNextChar of 
                  '=' ->  addToken (advanceLexer checkNextChar) Token {tokenType = lessEqOperation definedTypes, val = Nothing, pos = currentPosition lexer }
                  '-' -> addToken (advanceLexer checkNextChar) Token {tokenType = closeStatement definedTypes, val = Nothing, pos = currentPosition lexer }
                  _  -> addToken (advanceLexer lexer) Token {tokenType = lessOperation definedTypes, val = Nothing, pos = currentPosition lexer }
              | currentChar lexer == '>' =
                case currentChar checkNextChar of 
                  '=' ->  addToken (advanceLexer checkNextChar) Token {tokenType = greaterEqOperation definedTypes, val = Nothing, pos = currentPosition lexer }
                  _  -> addToken (advanceLexer lexer) Token {tokenType = greaterOperation definedTypes, val = Nothing, pos = currentPosition lexer }
              | otherwise = advanceLexer lexer{lexerError = throwError (lexerError lexer) (InvalidCharError (fileName lexer) ( "\"" ++ [currentChar lexer] ++ "\"") (currentPosition lexer))}
            checkNextChar = advanceLexer lexer 


addToken :: Lexer -> Token -> Lexer
addToken lexer token = 
  lexer {tokenList = tokenList lexer ++ [token]}

makeString :: Lexer -> Lexer 
makeString lexer = 
  addToken (snd val) (Token {tokenType = stringType definedTypes, val = Just(String (fst val)), pos = currentPosition lexer})
  where
    val = createString (advanceLexer lexer)
    createString :: Lexer -> (String, Lexer)
    createString lexer
      | (currentChar lexer) == '\"' = ("", advanceLexer lexer) 
      | (currentChar lexer) == '\\' = case (currentChar checkEscapeChar) of
        '0' -> ((['\0'] ++ fst nextCharAfterEscape), (snd nextCharAfterEscape)) 
        'a' -> ((['\a'] ++ fst nextCharAfterEscape), (snd nextCharAfterEscape))
        'b' -> ((['\b'] ++ fst nextCharAfterEscape), (snd nextCharAfterEscape))
        'f' -> ((['\f'] ++ fst nextCharAfterEscape), (snd nextCharAfterEscape))
        'n' -> ((['\n'] ++ fst nextCharAfterEscape), (snd nextCharAfterEscape))
        'r' -> ((['\r'] ++ fst nextCharAfterEscape), (snd nextCharAfterEscape))
        't' -> ((['\t'] ++ fst nextCharAfterEscape), (snd nextCharAfterEscape))
        'v' -> ((['\v'] ++ fst nextCharAfterEscape), (snd nextCharAfterEscape))
        '&' -> (("\&" ++ fst nextCharAfterEscape), (snd nextCharAfterEscape))
        '\'' -> ((['\''] ++ fst nextCharAfterEscape), (snd nextCharAfterEscape))
        '\\' -> ((['\\'] ++ fst nextCharAfterEscape), (snd nextCharAfterEscape))
        _ -> ("", lexer{lexerError = throwError (lexerError lexer) (InvalidCharError (fileName lexer) ( "\"" ++ [currentChar lexer] ++ [currentChar checkEscapeChar] ++ "\"") (currentPosition lexer))})
      | isPrint(currentChar lexer) = (([currentChar lexer] ++ fst nextChar), (snd nextChar))
      | otherwise = ("", lexer{lexerError = throwError (lexerError lexer) (InvalidSyntaxError  (fileName lexer) "\""  ( "\"" ++ [currentChar lexer] ++ "\"") (currentPosition lexer))})
      where 
        checkEscapeChar = advanceLexer lexer
        nextCharAfterEscape = createString (advanceLexer checkEscapeChar)
        nextChar = createString (advanceLexer lexer)

makeWord :: Lexer -> Lexer 
makeWord lexer = 
  makeLetters lexer "" (currentPosition lexer) where 
    makeLetters :: Lexer -> String -> Position -> Lexer
    makeLetters lexer string pos 
      | isDigit(currentChar lexer) || isAlpha(currentChar lexer) || (currentChar lexer == '_') =
        makeLetters (advanceLexer lexer) (string ++ [currentChar lexer]) pos
      | otherwise = addToken lexer (getKeyWord string pos)

getKeyWord :: String -> Position -> Token 
getKeyWord keyword pos
  | keyword == sqrootOperation definedTypes = Token{tokenType = sqrootOperation definedTypes, val = Nothing, pos = pos}
  | keyword == ifOperation definedTypes = Token{tokenType = ifOperation definedTypes, val = Nothing, pos = pos}
  | keyword == elseIfOperation definedTypes = Token{tokenType = elseIfOperation definedTypes, val = Nothing, pos = pos}
  | keyword == elseOperation definedTypes = Token{tokenType = elseOperation definedTypes, val = Nothing, pos = pos}
  | keyword == trueBool definedTypes = Token{tokenType = trueBool definedTypes, val = Just(Bool(True)), pos = pos}
  | keyword == falseBool definedTypes = Token{tokenType = falseBool definedTypes, val = Just(Bool(False)), pos = pos}
  | keyword == falseBool definedTypes = Token{tokenType = falseBool definedTypes, val = Just(Bool(False)), pos = pos}
  | keyword == nullType definedTypes = Token{tokenType = nullType definedTypes, val = Nothing, pos = pos}
  | keyword == loopOperation definedTypes = Token{tokenType = loopOperation definedTypes, val = Nothing, pos = pos}
  | keyword == fromLoopOperation definedTypes = Token{tokenType = fromLoopOperation definedTypes, val = Nothing, pos = pos}
  | keyword == toLoopOperation definedTypes = Token{tokenType = toLoopOperation definedTypes, val = Nothing, pos = pos}
  | keyword == withLoopOperation definedTypes = Token{tokenType = withLoopOperation definedTypes, val = Nothing, pos = pos}
  | keyword == untilOperation definedTypes = Token{tokenType = untilOperation definedTypes, val = Nothing, pos = pos}
  | keyword == function definedTypes = Token{tokenType = function definedTypes, val = Nothing, pos = pos}
  | keyword == returnFunction definedTypes = Token{tokenType = returnFunction definedTypes, val = Nothing, pos = pos}
  | keyword == runFunction definedTypes = Token{tokenType = runFunction definedTypes, val = Nothing, pos = pos}
  | keyword == importFunction definedTypes = Token{tokenType = importFunction definedTypes, val = Nothing, pos = pos}
  | keyword == printFunction definedTypes = Token{tokenType = printFunction definedTypes, val = Nothing, pos = pos}
  | otherwise = Token{tokenType = identifier definedTypes, val = Just(String(keyword)), pos = pos}


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
