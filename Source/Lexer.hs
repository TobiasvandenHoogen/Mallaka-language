{- 
Module      : Lexer
Description : The lexer of the interpreter which converts raw input into tokens. 
Maintainer  : Tobias van den Hoogen  
-}
{-# LANGUAGE DerivingStrategies #-}
module Source.Lexer(module Source.Lexer) where

import Data.Char ( isAlpha, isDigit, isPrint )
import Data.Maybe()
import Data.Bifunctor ( Bifunctor(first) )
import Source.Exception
import Source.Types

-- | The Lexer of the Mallaka Language
data Lexer = Lexer{
    fileName :: String,
    inputText :: String,
    currentLine :: Int,
    currentPosition :: Position,
    currentChar :: Char,
    lexerError :: Error,
    tokenList :: [Token]
    } deriving stock (Show)

advancePosition :: Position -> Char -> Position
advancePosition position currentCharacter =
    let checkColumn = if currentCharacter == '\n' then 0 else posColumn position in
    let checkLine = if currentCharacter == '\n' then posLine position + 1  else posLine position in
    Position{ posIndex = posIndex position + 1,
              posColumn = checkColumn,
              posLine = checkLine}

advanceLexer :: Lexer -> Lexer
advanceLexer lexer =
    let char =
            if posIndex (currentPosition lexer) < length (inputText lexer)
            then take (posIndex (currentPosition lexer)) (drop (posIndex (currentPosition lexer)) (inputText lexer)  )
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
      then addToken lexer Token {tokenType = endOfFile definedTypes, tokenVal = Nothing , tokenPos = currentPosition lexer}
      else tokens where
        tokens =
          createTokens newLexer where
            newLexer
              | currentChar lexer == '@' = lexComments lexer
              | isDigit (currentChar lexer) = makeNumber lexer
              | isAlpha (currentChar lexer) = makeWord lexer
              | currentChar lexer == ' ' = advanceLexer lexer
              | currentChar lexer == '\n' = advanceLexer lexer
              | currentChar lexer == '+' = addToken (advanceLexer lexer) Token {tokenType = plusOperation definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer}
              | currentChar lexer == '-' =
                case currentChar checkNextChar of
                  '>' ->  addToken (advanceLexer checkNextChar) Token {tokenType = openStatement definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
                  _  ->   addToken (advanceLexer lexer) Token {tokenType = minusOperation definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
              | currentChar lexer == '*' = addToken (advanceLexer lexer) Token {tokenType = multiplyOperation definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
              | currentChar lexer == '/' = addToken (advanceLexer lexer) Token {tokenType = divisionOperation definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
              | currentChar lexer == '%' = addToken (advanceLexer lexer) Token {tokenType = modOperation definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
              | currentChar lexer == '^' = addToken (advanceLexer lexer) Token {tokenType = powerOperation definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
              | currentChar lexer == '$' = addToken (advanceLexer lexer) Token {tokenType = indexOperation definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer}
              | currentChar lexer == '=' =
                if currentChar checkNextChar == '='
                then addToken (advanceLexer checkNextChar) Token {tokenType = equalOperation definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
                else addToken (advanceLexer lexer) Token {tokenType = assignOperation definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
              | currentChar lexer == '(' && currentChar checkNextChar == ')' = addToken (advanceLexer checkNextChar) Token {tokenType = nullType definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer}
              | currentChar lexer == '(' = addToken (advanceLexer lexer) Token {tokenType = leftParent definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
              | currentChar lexer == ')' = addToken (advanceLexer lexer) Token {tokenType = rightParent definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
              | currentChar lexer == '[' = addToken (advanceLexer lexer) Token {tokenType = openList definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
              | currentChar lexer == ']' = addToken (advanceLexer lexer) Token {tokenType = closeList definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
              | currentChar lexer == '\"' = makeString lexer
              | currentChar lexer == '{' = addToken (advanceLexer lexer) Token {tokenType = openParameter definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
              | currentChar lexer == '}' = addToken (advanceLexer lexer) Token {tokenType = closeParameter definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
              | currentChar lexer == ',' = addToken (advanceLexer lexer) Token {tokenType = separatorParameter definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer}
              | currentChar lexer == '&' = addToken (advanceLexer lexer) Token {tokenType = andOperation definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
              | currentChar lexer == '|' = addToken (advanceLexer lexer) Token {tokenType = orOperation definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
              | currentChar lexer == ';' = addToken (advanceLexer lexer) Token {tokenType = statementSeparator definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer}
              | currentChar lexer == '!' =
                if currentChar checkNextChar == '='
                then addToken (advanceLexer checkNextChar) Token {tokenType = notEqualOperation definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
                else addToken (advanceLexer lexer) Token {tokenType = notOperation definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
              | currentChar lexer == '<' =
                case currentChar checkNextChar of
                  '=' ->  addToken (advanceLexer checkNextChar) Token {tokenType = lessEqOperation definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
                  '-' -> addToken (advanceLexer checkNextChar) Token {tokenType = closeStatement definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
                  _  -> addToken (advanceLexer lexer) Token {tokenType = lessOperation definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
              | currentChar lexer == '>' =
                case currentChar checkNextChar of
                  '=' ->  addToken (advanceLexer checkNextChar) Token {tokenType = greaterEqOperation definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
                  _  -> addToken (advanceLexer lexer) Token {tokenType = greaterOperation definedTypes, tokenVal = Nothing, tokenPos = currentPosition lexer }
              | otherwise = advanceLexer lexer{lexerError = throwError (lexerError lexer) (InvalidCharError (fileName lexer) ( "\"" ++ [currentChar lexer] ++ "\"") (currentPosition lexer))}
            checkNextChar = advanceLexer lexer


lexComments :: Lexer -> Lexer
lexComments lexer
  | currentChar lexer /= '\n' = lexComments (advanceLexer lexer)
  | otherwise = lexer


addToken :: Lexer -> Token -> Lexer
addToken lexer token =
  lexer {tokenList = tokenList lexer ++ [token]}

makeString :: Lexer -> Lexer
makeString lexer =
  addToken (snd tokenValue) (Token {tokenType = stringType definedTypes, tokenVal = Just(StringValue (fst tokenValue)), tokenPos = currentPosition lexer})
  where
    tokenValue = createString (advanceLexer lexer)
    createString :: Lexer -> (String, Lexer)
    createString lexer2
      | currentChar lexer2 == '\"' = ("", advanceLexer lexer2)
      | currentChar lexer2 == '\\' = case currentChar checkEscapeChar of
        '0' -> first (['\0'] ++) nextCharAfterEscape
        'a' -> first (['\a'] ++) nextCharAfterEscape
        'b' -> first (['\b'] ++) nextCharAfterEscape
        'f' -> first (['\f'] ++) nextCharAfterEscape
        'n' -> first (['\n'] ++) nextCharAfterEscape
        'r' -> first (['\r'] ++) nextCharAfterEscape
        't' -> first (['\t'] ++) nextCharAfterEscape
        'v' -> first (['\v'] ++) nextCharAfterEscape
        '&' -> first ("\&" ++) nextCharAfterEscape
        '\'' -> first (['\''] ++) nextCharAfterEscape
        '\\' -> first (['\\'] ++) nextCharAfterEscape
        _ -> ("", lexer2{lexerError = throwError (lexerError lexer2) (InvalidCharError (fileName lexer2) ( "\"" ++ [currentChar lexer2] ++ [currentChar checkEscapeChar] ++ "\"") (currentPosition lexer2))})
      | isPrint(currentChar lexer2) = first ([currentChar lexer2] ++) nextChar
      | otherwise = ("", lexer2{lexerError = throwError (lexerError lexer2) (InvalidSyntaxError  (fileName lexer2) "\""  ( "\"" ++ [currentChar lexer2] ++ "\"") (currentPosition lexer2))})
      where
        checkEscapeChar = advanceLexer lexer2
        nextCharAfterEscape = createString (advanceLexer checkEscapeChar)
        nextChar = createString (advanceLexer lexer2)

makeWord :: Lexer -> Lexer
makeWord lexer =
  makeLetters lexer "" (currentPosition lexer) where
    makeLetters :: Lexer -> String -> Position -> Lexer
    makeLetters lexer2 string position
      | isDigit(currentChar lexer2) || isAlpha(currentChar lexer2) || (currentChar lexer2 == '_') =
        makeLetters (advanceLexer lexer2) (string ++ [currentChar lexer2]) position
      | otherwise = addToken lexer2 (getKeyWord string position)

getKeyWord :: String -> Position -> Token
getKeyWord keyword position
  | keyword == sqrootOperation definedTypes = Token{tokenType = sqrootOperation definedTypes, tokenVal = Nothing, tokenPos = position}
  | keyword == ifOperation definedTypes = Token{tokenType = ifOperation definedTypes, tokenVal = Nothing, tokenPos = position}
  | keyword == elseIfOperation definedTypes = Token{tokenType = elseIfOperation definedTypes, tokenVal = Nothing, tokenPos = position}
  | keyword == elseOperation definedTypes = Token{tokenType = elseOperation definedTypes, tokenVal = Nothing, tokenPos = position}
  | keyword == trueBool definedTypes = Token{tokenType = trueBool definedTypes, tokenVal = Just(BoolValue True), tokenPos = position}
  | keyword == falseBool definedTypes = Token{tokenType = falseBool definedTypes, tokenVal = Just(BoolValue False), tokenPos = position}
  | keyword == falseBool definedTypes = Token{tokenType = falseBool definedTypes, tokenVal = Just(BoolValue False), tokenPos = position}
  | keyword == nullType definedTypes = Token{tokenType = nullType definedTypes, tokenVal = Nothing, tokenPos = position}
  | keyword == loopOperation definedTypes = Token{tokenType = loopOperation definedTypes, tokenVal = Nothing, tokenPos = position}
  | keyword == fromLoopOperation definedTypes = Token{tokenType = fromLoopOperation definedTypes, tokenVal = Nothing, tokenPos = position}
  | keyword == toLoopOperation definedTypes = Token{tokenType = toLoopOperation definedTypes, tokenVal = Nothing, tokenPos = position}
  | keyword == withLoopOperation definedTypes = Token{tokenType = withLoopOperation definedTypes, tokenVal = Nothing, tokenPos = position}
  | keyword == untilOperation definedTypes = Token{tokenType = untilOperation definedTypes, tokenVal = Nothing, tokenPos = position}
  | keyword == function definedTypes = Token{tokenType = function definedTypes, tokenVal = Nothing, tokenPos = position}
  | keyword == runFunction definedTypes = Token{tokenType = runFunction definedTypes, tokenVal = Nothing, tokenPos = position}
  | keyword == importFunction definedTypes = Token{tokenType = importFunction definedTypes, tokenVal = Nothing, tokenPos = position}
  | keyword == printFunction definedTypes = Token{tokenType = printFunction definedTypes, tokenVal = Nothing, tokenPos = position}
  | otherwise = Token{tokenType = identifier definedTypes, tokenVal = Just(StringValue keyword), tokenPos = position}


makeNumber :: Lexer -> Lexer
makeNumber lexer =
  makeNumbers lexer 0 "" where
    makeNumbers :: Lexer -> Int -> String -> Lexer
    makeNumbers lexer2 dotCount numberString
      | (currentChar lexer2 == '.') && (dotCount == 0) =
        makeNumbers (advanceLexer lexer2) (dotCount + 1) (numberString ++ [currentChar lexer2])
      | isDigit(currentChar lexer2) =
        makeNumbers (advanceLexer lexer2) dotCount (numberString ++ [currentChar lexer2])
      | otherwise = 
        if dotCount == 0
        then
          addToken lexer2 Token{tokenType = intType definedTypes, tokenVal = Just(IntValue(read numberString :: Int)), tokenPos = currentPosition lexer2}
        else
          addToken lexer2 Token{tokenType = floatType definedTypes, tokenVal = Just(FloatValue(read numberString :: Float)), tokenPos = currentPosition lexer2}
