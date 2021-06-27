module Parser where 
import Data.Maybe
import Exception 
import Types 

----------Parser

data Node = Node{
  leftNode :: Maybe Node,
  token :: Token,
  rightNode :: Maybe Node,
  nodeType :: String}
  deriving Show  

data Parser = Parser{
  tokens :: [Token],
  tokenIndex :: Int,
  currentToken :: Token,
  currentNode :: Maybe Node,
  file :: String }
  deriving Show 

advanceParser :: Parser -> Parser
advanceParser parser = 
    let newToken = 
            if tokenIndex parser < length (tokens parser)
            then head (take (tokenIndex parser) (drop (tokenIndex parser) (tokens parser) ))
            else currentToken parser
        result = Parser {tokens = tokens parser,
                        tokenIndex = tokenIndex parser + 1,
                        currentToken = newToken,
                        currentNode = currentNode parser,
                        file = file parser}
    in result  

factor :: Parser -> Parser 
factor parser 
  | (tokenType (currentToken parser) == intType definedTypes) || (tokenType (currentToken parser) == floatType definedTypes) = 
    advanceParser parser { currentNode = Just Node{leftNode = Nothing, token = currentToken parser, rightNode = Nothing, nodeType = "NumberNode"}}
  | (tokenType (currentToken parser) == plusOperation definedTypes) || (tokenType (currentToken parser) == minusOperation definedTypes) = 
    newParser  { currentNode = Just Node{leftNode = currentNode newParser, token = currentToken parser, rightNode = Nothing, nodeType = "UnaryNode"}}
  | tokenType (currentToken parser) == leftParent definedTypes = 
    if tokenType (currentToken expressionParser) == rightParent definedTypes
    then advanceParser expressionParser 
    else throwError(InvalidSyntaxError (file parser) ")" ("\"" ++ tokenType (currentToken expressionParser) ++ "\"") (pos (currentToken expressionParser)))
  | otherwise = 
    throwError(InvalidSyntaxError (file parser) "integer/float" ("\"" ++ tokenType (currentToken parser) ++ "\"") (pos (currentToken parser)))
  where 
      newParser = factor (advanceParser parser)
      expressionParser = expression (advanceParser parser) Nothing 


term :: Parser -> Maybe Node -> Parser
term parser node  
  | isNothing node = term newParser (currentNode newParser)
  | (tokenType (currentToken parser) == multiplyOperation definedTypes) || (tokenType (currentToken parser) == divisionOperation definedTypes) =
    term returnParser (Just (Node{leftNode = node, token = currentToken parser, rightNode = currentNode returnParser, nodeType = "BinaryOpNode"}))
  | otherwise = parser{currentNode = node}
  where
    newParser = factor parser 
    newNewParser = advanceParser parser
    returnParser = factor newNewParser


expression :: Parser -> Maybe Node -> Parser 
expression parser node  
  | isNothing node = expression newParser (currentNode newParser)
  | (tokenType (currentToken parser) == plusOperation definedTypes) || (tokenType (currentToken parser) == minusOperation definedTypes) =
    expression returnParser (Just (Node{leftNode = node, token = currentToken parser, rightNode = currentNode returnParser, nodeType = "BinaryOpNode"}))
  | otherwise = parser{currentNode = node}
  where
    newParser = term parser Nothing 
    newNewParser = advanceParser parser
    returnParser = term newNewParser Nothing

parse :: Parser -> Parser 
parse parser = 
    if tokenType (currentToken newParser) == "EOF"
    then newParser 
    else throwError(InvalidSyntaxError (file parser) "operator" ("\"" ++ tokenType (currentToken parser) ++ "\"") (pos (currentToken parser)))
    where
      newParser = expression parser Nothing
