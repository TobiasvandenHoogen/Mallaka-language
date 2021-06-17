
import System.IO
import Data.Char
import Data.Maybe
---------Data

type Ident = String 

data Value = IntVal Int |
  FloatVal Float
  deriving Show

data Types = Types{intType :: String, 
        floatType :: String,
        plusOperation :: String,
        minusOperation :: String,
        multiplyOperation :: String,
        divisionOperation :: String,
        leftParent :: String,
        rightParent :: String}
        


definedTypes :: Types 
definedTypes = Types{intType = "Integer",
                 floatType = "Float",
                 plusOperation = "Plus",
                 minusOperation = "Minus",
                 multiplyOperation = "Multiply",
                 divisionOperation = "Division",
                 leftParent = "LParent",
                 rightParent = "RParent" }

-----------Tokens

data Token = Token{ tokenType :: String,
        val :: Maybe Value}
        deriving Show 

splitInput :: String -> [String]
splitInput "hallo" = ["hallo"]
splitInput "doei" = ["doei"]

data Error = InvalidCharError String String Position

throwError :: Error -> Lexer
throwError (InvalidCharError fn err pos) = error ( 
  "\n" ++ fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " ++ "Error: invalid character: " ++ err)


-- Map tokens = [("hallo", 8)]

----------Lexer

data Position = Position {index :: Int,
  line :: Int,
  column :: Int}

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
      then lexer
      else tokens where 
        tokens =  
          createTokens newLexer where 
            newLexer
              | isDigit (currentChar lexer) = makeNumber lexer 
              | currentChar lexer == ' ' = advanceLexer lexer 
              | currentChar lexer == '+' = addToken (advanceLexer lexer) Token {tokenType = plusOperation definedTypes, val = Nothing}
              | currentChar lexer == '-' = addToken (advanceLexer lexer) Token {tokenType = minusOperation definedTypes, val = Nothing }
              | currentChar lexer == '*' = addToken (advanceLexer lexer) Token {tokenType = multiplyOperation definedTypes, val = Nothing }
              | currentChar lexer == '/' = addToken (advanceLexer lexer) Token {tokenType = divisionOperation definedTypes, val = Nothing }
              | currentChar lexer == '(' = addToken (advanceLexer lexer) Token {tokenType = leftParent definedTypes, val = Nothing }
              | currentChar lexer == ')' = addToken (advanceLexer lexer) Token {tokenType = rightParent definedTypes, val = Nothing }
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
          addToken lexer Token{tokenType = intType definedTypes, val = Just(IntVal(read numberString :: Int))}
        else
          addToken lexer Token{tokenType = floatType definedTypes, val = Just(FloatVal(read numberString :: Float))}

        



 

----------Parser

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
  print (tokenList tok)
  print (currentChar tok)
  print (inputText tok)
  print (index (currentPosition tok))
  main 
  return ()
