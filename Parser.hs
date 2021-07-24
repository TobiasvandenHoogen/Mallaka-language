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

statementExpression :: Parser -> Parser 
statementExpression parser 
  | tokenType (currentToken parser) == openStatement definedTypes =  
    case tokenType (currentToken statement) of 
      "<-" -> advanceParser statement
      _ ->  throwError(InvalidSyntaxError (file parser) (closeStatement definedTypes) ("\"" ++ tokenType (currentToken parser) ++ "\"") (pos (currentToken parser)))
  | otherwise = throwError(InvalidSyntaxError (file parser) (openStatement definedTypes) ("\"" ++ tokenType (currentToken parser) ++ "\"") (pos (currentToken parser)))
  where 
    statement = expression (advanceParser parser) Nothing 
  

ifExpression :: Parser -> Parser 
ifExpression parser 
  | tokenType (currentToken parser) == ifOperation definedTypes ||
    tokenType (currentToken parser) == elseIfOperation definedTypes  = otherStatements {currentNode = Just ifTree}
  | tokenType (currentToken parser) == elseOperation definedTypes = elseStatement {currentNode = Just elseNode}
  | otherwise = parser {currentNode = Nothing}
  where 
    condition = exponential (advanceParser parser) Nothing
    statement = statementExpression condition
    otherStatements = ifExpression statement
    elseStatement = statementExpression (advanceParser parser)
    ifTree = Node{leftNode = Just ifNode, token = currentToken parser, rightNode = currentNode otherStatements, nodeType = "IfTreeNode"}
    ifNode = Node{leftNode = currentNode condition, token = currentToken parser, rightNode =  currentNode statement, nodeType = "IfOpNode"}
    elseNode = Node{leftNode = Nothing, token = currentToken parser, rightNode =  currentNode elseStatement, nodeType = "ElseOpNode"}


atom :: Parser -> Parser  
atom parser 
  | (tokenType (currentToken parser) == intType definedTypes) || 
    (tokenType (currentToken parser) == floatType definedTypes) ||
    (tokenType (currentToken parser) == nullType definedTypes) ||
    (tokenType (currentToken parser) == trueBool definedTypes) ||
    (tokenType (currentToken parser) == falseBool definedTypes) = 
      advanceParser parser { currentNode = Just Node{leftNode = Nothing, token = currentToken parser, rightNode = Nothing, nodeType = "NumberNode"}}
  | tokenType (currentToken parser) == identifier definedTypes =  
    advanceParser parser { currentNode = Just Node{leftNode = Nothing, token = currentToken parser, rightNode = Nothing, nodeType = "VarAccessNode"}}
  | tokenType (currentToken parser) == leftParent definedTypes = 
    if tokenType (currentToken expressionParser) == rightParent definedTypes
    then advanceParser expressionParser 
    else throwError(InvalidSyntaxError (file parser) ")" ("\"" ++ tokenType (currentToken expressionParser) ++ "\"") (pos (currentToken expressionParser)))
  | tokenType (currentToken parser) == ifOperation definedTypes = 
    ifExpression parser 
  | otherwise = 
    throwError(InvalidSyntaxError (file parser) "value type" ("\"" ++ tokenType (currentToken parser) ++ "\"") (pos (currentToken parser)))
  where 
    expressionParser = expression (advanceParser parser) Nothing 

factor :: Parser -> Parser 
factor parser 
  | (tokenType (currentToken parser) == plusOperation definedTypes) || 
    (tokenType (currentToken parser) == minusOperation definedTypes) ||
    (tokenType (currentToken parser) == notOperation definedTypes) =  
    newParser  { currentNode = Just Node{leftNode = currentNode newParser, token = currentToken parser, rightNode = Nothing, nodeType = "UnaryNode"}}
  | otherwise = atom parser 
  where 
    newParser = atom (advanceParser parser)
  
exponential :: Parser -> Maybe Node -> Parser 
exponential parser node 
  | tokenType (currentToken parser) == sqrootOperation definedTypes  =
    term returnParser (Just (Node{leftNode = Nothing, token = currentToken parser, rightNode = currentNode returnParser, nodeType = "BinaryOpNode"}))
  | isNothing node = exponential newParser (currentNode newParser)
  | tokenType (currentToken parser) == powerOperation definedTypes  =
      term returnParser (Just (Node{leftNode = node, token = currentToken parser, rightNode = currentNode returnParser, nodeType = "BinaryOpNode"}))
  | otherwise = parser{currentNode = node}
  where
    newParser = factor parser 
    newNewParser = advanceParser parser
    returnParser = factor newNewParser

term :: Parser -> Maybe Node -> Parser
term parser node  
  | isNothing node = term newParser (currentNode newParser)
  | (tokenType (currentToken parser) == multiplyOperation definedTypes) 
    || (tokenType (currentToken parser) == divisionOperation definedTypes) 
    || (tokenType (currentToken parser) == modOperation definedTypes) =
      term returnParser (Just (Node{leftNode = node, token = currentToken parser, rightNode = currentNode returnParser, nodeType = "BinaryOpNode"}))
  | otherwise = parser{currentNode = node}
  where
    newParser = exponential parser Nothing 
    newNewParser = advanceParser parser
    returnParser = exponential newNewParser Nothing 

arithmeticExpression :: Parser -> Maybe Node -> Parser 
arithmeticExpression parser node 
  | isNothing node = arithmeticExpression newParser (currentNode newParser)
  | (tokenType (currentToken parser) == plusOperation definedTypes) || (tokenType (currentToken parser) == minusOperation definedTypes) =
    arithmeticExpression returnParser (Just (Node{leftNode = node, token = currentToken parser, rightNode = currentNode returnParser, nodeType = "BinaryOpNode"}))
  | otherwise = parser{currentNode = node}
  where
    newParser = term parser Nothing 
    newNewParser = advanceParser parser
    returnParser = term newNewParser Nothing

compareExpression :: Parser -> Maybe Node -> Parser 
compareExpression parser node 
  | isNothing node = compareExpression newParser (currentNode newParser)
  | (tokenType (currentToken parser) == equalOperation definedTypes) || 
    (tokenType (currentToken parser) == notEqualOperation definedTypes) || 
    (tokenType (currentToken parser) == lessOperation definedTypes) ||
    (tokenType (currentToken parser) == greaterOperation definedTypes) || 
    (tokenType (currentToken parser) == lessEqOperation definedTypes) ||
    (tokenType (currentToken parser) == greaterEqOperation definedTypes) = 
    compareExpression returnParser (Just (Node{leftNode = node, token = currentToken parser, rightNode = currentNode returnParser, nodeType = "BinaryOpNode"}))
  | otherwise = parser{currentNode = node}
  where
    newParser = arithmeticExpression parser Nothing 
    newNewParser = advanceParser parser
    returnParser = arithmeticExpression newNewParser Nothing

logicalExpression :: Parser -> Maybe Node -> Parser 
logicalExpression parser node 
  | isNothing node = logicalExpression newParser (currentNode newParser)
  | (tokenType (currentToken parser) == andOperation definedTypes) || (tokenType (currentToken parser) == orOperation definedTypes) =
    logicalExpression returnParser (Just (Node{leftNode = node, token = currentToken parser, rightNode = currentNode returnParser, nodeType = "BinaryOpNode"}))
  | otherwise = parser{currentNode = node}
  where
    newParser = compareExpression parser Nothing 
    newNewParser = advanceParser parser
    returnParser = compareExpression newNewParser Nothing

expression :: Parser -> Maybe Node -> Parser 
expression parser node  
  | isNothing node = expression newParser (currentNode newParser)
  | tokenType (currentToken parser) == assignOperation definedTypes =
    if tokenType (token varNode) == identifier definedTypes
    then expression returnParser (Just (Node{leftNode = node, token = currentToken parser, rightNode = currentNode returnParser, nodeType = "AssignNode"}))
    else throwError(InvalidSyntaxError (file parser) "identifier" ("\"" ++ tokenType (token varNode) ++ "\"") (pos (token varNode)))
  | (tokenType (currentToken parser) == plusOperation definedTypes) || (tokenType (currentToken parser) == minusOperation definedTypes) =
    expression returnParser (Just (Node{leftNode = node, token = currentToken parser, rightNode = currentNode returnParser, nodeType = "BinaryOpNode"}))
  | otherwise = parser{currentNode = node}
  where
    varNode = fromJust node
    newParser = logicalExpression parser Nothing 
    newNewParser = advanceParser parser
    returnParser = logicalExpression newNewParser Nothing

parse :: Parser -> Parser 
parse parser = 
    if tokenType (currentToken newParser) == "EOF"
    then newParser 
    else throwError(InvalidSyntaxError (file parser) "operator" ("\"" ++ tokenType (currentToken parser) ++ "\"") (pos (currentToken parser)))
    where
      newParser = expression parser Nothing

