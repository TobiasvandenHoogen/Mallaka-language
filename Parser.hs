module Parser where 
import Data.Maybe
import Exception 
import Types 

----------Parser

data Parser = Parser{
  tokens :: [Token],
  tokenIndex :: Int,
  currentToken :: Token,
  currentNode :: Node,
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
    statement = expression (advanceParser parser) Empty
  

ifExpression :: Parser -> Parser 
ifExpression parser 
  | tokenType (currentToken parser) == ifOperation definedTypes ||
    tokenType (currentToken parser) == elseIfOperation definedTypes  = otherStatements {currentNode = ifTree}
  | tokenType (currentToken parser) == elseOperation definedTypes = elseStatement {currentNode = elseNode}
  | otherwise = parser {currentNode = Empty}
  where 
    condition = expression (advanceParser parser) Empty
    statement = statementExpression condition
    otherStatements = ifExpression statement
    elseStatement = statementExpression (advanceParser parser)
    ifTree = Tree ifNode (currentToken parser) IfNode (currentNode otherStatements)
    ifNode = Tree (currentNode condition) (currentToken parser) NoType (currentNode statement)
    elseNode = Tree (currentNode elseStatement) (currentToken parser) NoType Empty

loopExpression :: Parser -> Parser 
loopExpression parser 
  | tokenType (currentToken parser) == loopOperation definedTypes = statementBranch{currentNode = loopBranch}
  | tokenType (currentToken parser) == fromLoopOperation definedTypes ||
    tokenType (currentToken parser) == toLoopOperation definedTypes ||
    tokenType (currentToken parser) == withLoopOperation definedTypes = 
      loopAttribute { currentNode = Branch (currentToken parser) LoopNode (currentNode loopAttribute)}
  | otherwise = throwError(InvalidSyntaxError (file parser) "loop keyword" ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (pos (currentToken parser)))
  where 
    loopBranch = Tree
                  (Tree (currentNode fromBranch) (currentToken parser) NoType (currentNode toBranch))
                  (currentToken parser)
                  LoopNode
                  (Tree (currentNode withBranch) (currentToken parser) NoType (currentNode statementBranch))
    loopAttribute = expression (advanceParser parser) Empty
    fromBranch = loopExpression(advanceParser parser)
    toBranch = loopExpression fromBranch
    withBranch = loopExpression toBranch
    statementBranch = statementExpression withBranch

untilExpression :: Parser -> Parser 
untilExpression parser = statementBranch{currentNode = untilBranch} 
  where 
    untilBranch = Tree (currentNode conditionBranch) (currentToken parser) UntilNode (currentNode statementBranch)
    conditionBranch = expression (advanceParser parser) Empty
    statementBranch = statementExpression conditionBranch

runFunctionExpression :: Parser -> Parser 
runFunctionExpression parser 
  | tokenType (currentToken parser) == runFunction definedTypes =
    if tokenType (currentToken (advanceParser parser)) == identifier definedTypes
    then newParser{currentNode = functionTree}
    else throwError(InvalidSyntaxError (file parser) "identifier" ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (pos (currentToken parser)))
  | otherwise = throwError(InvalidSyntaxError (file parser) "function keyword" ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (pos (currentToken parser)))
  where 
    identLeaf = Leaf (currentToken (advanceParser parser)) NoType 
    functionTree = Tree identLeaf (currentToken parser) FunctionRunNode (currentNode newParser) 
    newParser = runParameterExpression (advanceParser (advanceParser parser))

runParameterExpression :: Parser -> Parser 
runParameterExpression parser 
  | tokenType (currentToken parser) == openParameter definedTypes = parameterParser
  | otherwise = throwError(InvalidSyntaxError (file parser) "function keyword" ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (pos (currentToken parser)))
  where
    parameterParser = runParameters(advanceParser parser)

runParameters :: Parser -> Parser 
runParameters parser 
  | tokenType (currentToken parser) == closeParameter definedTypes = advanceParser parser
  | tokenType (currentToken getParameter) == seperatorParameter definedTypes = endParameter{currentNode = parameterTree}
  | tokenType (currentToken getParameter) == closeParameter definedTypes = (advanceParser getParameter){currentNode = endParameterTree}
  | otherwise = throwError(InvalidParameterName (file parser) ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (pos (currentToken parser)))
  where 
    getParameter = (atom (parser))
    parameterTree = Branch (currentToken parser) NoType (currentNode nextParameter)
    endParameterTree = Branch (currentToken parser) NoType Empty 
    nextParameter = runParameters (advanceParser getParameter)
    endParameter = nextParameter
    
createFunctionExpression :: Parser -> Parser 
createFunctionExpression parser 
  | tokenType (currentToken parser) == function definedTypes = 
    if tokenType (currentToken (advanceParser parser)) == identifier definedTypes
    then newParser{currentNode = functionTree}
    else throwError(InvalidSyntaxError (file parser) "identifier" ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (pos (currentToken parser)))
  | otherwise = throwError(InvalidSyntaxError (file parser) "function keyword" ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (pos (currentToken parser)))
  where 
    varLeaf = Leaf (currentToken (advanceParser parser)) NoType
    functionTree = Tree varLeaf (currentToken parser) FunctionAssignNode (currentNode newParser) 
    newParser = createParameterExpression (advanceParser (advanceParser parser))
  
createParameterExpression :: Parser -> Parser 
createParameterExpression parser 
  | tokenType (currentToken parser) == openParameter definedTypes = statementParser{currentNode = inputBranch} 
  | otherwise = throwError(InvalidSyntaxError (file parser) "function keyword" ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (pos (currentToken parser)))
  where
    parameterParser = registerParameters(advanceParser parser)
    statementParser = statementExpression parameterParser
    inputBranch = Tree (currentNode parameterParser) (currentToken parser) NoType (currentNode statementParser)


registerParameters :: Parser -> Parser 
registerParameters parser 
  | tokenType (currentToken parser) == identifier definedTypes = case ( tokenType (currentToken checkSeperator)) of 
    "," -> endParameter{currentNode = registerParameter}
    "}" -> (advanceParser checkSeperator){currentNode = Branch (currentToken parser) NoType Empty}
  | tokenType (currentToken parser) == closeParameter definedTypes = advanceParser checkSeperator
  | otherwise = throwError(InvalidParameterName (file parser) ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (pos (currentToken parser)))
  where 
    checkSeperator = advanceParser parser
    registerParameter = Branch (currentToken parser) NoType (currentNode nextParameter)
    nextParameter = registerParameters(advanceParser checkSeperator)
    endParameter = nextParameter

    
atom :: Parser -> Parser  
atom parser 
  | (tokenType (currentToken parser) == intType definedTypes) || 
    (tokenType (currentToken parser) == floatType definedTypes) ||
    (tokenType (currentToken parser) == nullType definedTypes) ||
    (tokenType (currentToken parser) == trueBool definedTypes) ||
    (tokenType (currentToken parser) == falseBool definedTypes) = 
      advanceParser parser { currentNode = Leaf (currentToken parser) NumberNode}
  | tokenType (currentToken parser) == identifier definedTypes =  
    advanceParser parser { currentNode = Leaf (currentToken parser) VarAccessNode}
  | tokenType (currentToken parser) == leftParent definedTypes = 
    if tokenType (currentToken expressionParser) == rightParent definedTypes
    then advanceParser expressionParser 
    else throwError(InvalidSyntaxError (file parser) ")" ("\"" ++ tokenType (currentToken expressionParser) ++ "\"") (pos (currentToken expressionParser)))
  | tokenType (currentToken parser) == ifOperation definedTypes = 
    ifExpression parser 
  | tokenType (currentToken parser) == loopOperation definedTypes = 
    loopExpression parser
  | tokenType (currentToken parser) == untilOperation definedTypes = 
    untilExpression parser
  | tokenType (currentToken parser) == function definedTypes = 
    createFunctionExpression parser
  | tokenType (currentToken parser) == runFunction definedTypes = 
    runFunctionExpression parser
  | otherwise = 
    throwError(InvalidSyntaxError (file parser) "value" ("\"" ++ tokenType (currentToken parser) ++ "\"") (pos (currentToken parser)))
  where 
    expressionParser = expression (advanceParser parser) Empty

factor :: Parser -> Parser 
factor parser 
  | (tokenType (currentToken parser) == plusOperation definedTypes) || 
    (tokenType (currentToken parser) == minusOperation definedTypes) ||
    (tokenType (currentToken parser) == notOperation definedTypes) =  
    nextExpressionParser  { currentNode = Branch (currentToken parser) UnaryNode (currentNode nextExpressionParser)}
  | otherwise = atom parser 
  where 
    nextExpressionParser = atom (advanceParser parser)
  
exponential :: Parser -> Node -> Parser 
exponential parser node 
  | tokenType (currentToken parser) == sqrootOperation definedTypes  =
    term returnParser (Tree Empty (currentToken parser)  BinaryOpNode (currentNode returnParser))
  | node == Empty = exponential nextExpressionParser (currentNode nextExpressionParser)
  | tokenType (currentToken parser) == powerOperation definedTypes  =
      term returnParser (Tree node (currentToken parser) BinaryOpNode (currentNode returnParser))
  | otherwise = parser{currentNode = node}
  where
    nextExpressionParser = factor parser 
    returnParser = factor (advanceParser parser)

term :: Parser -> Node -> Parser
term parser node
  | node == Empty = term nextExpressionParser (currentNode nextExpressionParser)
  | (tokenType (currentToken parser) == multiplyOperation definedTypes) ||
    (tokenType (currentToken parser) == divisionOperation definedTypes) ||
    (tokenType (currentToken parser) == modOperation definedTypes) =
      term returnParser (Tree node (currentToken parser) BinaryOpNode (currentNode returnParser))
  | otherwise = parser{currentNode = node}
  where
    nextExpressionParser = exponential parser Empty
    returnParser = exponential (advanceParser parser) Empty

arithmeticExpression :: Parser -> Node -> Parser 
arithmeticExpression parser node 
  | node == Empty = arithmeticExpression nextExpressionParser (currentNode nextExpressionParser)
  | (tokenType (currentToken parser) == plusOperation definedTypes) || (tokenType (currentToken parser) == minusOperation definedTypes) =
    arithmeticExpression returnParser (Tree node (currentToken parser) BinaryOpNode (currentNode returnParser))
  | otherwise = parser{currentNode = node}
  where
    nextExpressionParser = term parser Empty
    returnParser = term (advanceParser parser) Empty

compareExpression :: Parser -> Node -> Parser 
compareExpression parser node 
  | node == Empty = compareExpression nextExpressionParser (currentNode nextExpressionParser)
  | (tokenType (currentToken parser) == equalOperation definedTypes) || 
    (tokenType (currentToken parser) == notEqualOperation definedTypes) || 
    (tokenType (currentToken parser) == lessOperation definedTypes) ||
    (tokenType (currentToken parser) == greaterOperation definedTypes) || 
    (tokenType (currentToken parser) == lessEqOperation definedTypes) ||
    (tokenType (currentToken parser) == greaterEqOperation definedTypes) = 
    compareExpression returnParser (Tree node (currentToken parser) BinaryOpNode (currentNode returnParser))
  | otherwise = parser{currentNode = node}
  where
    nextExpressionParser = arithmeticExpression parser Empty
    returnParser = arithmeticExpression (advanceParser parser) Empty

logicalExpression :: Parser -> Node -> Parser 
logicalExpression parser node 
  | node == Empty = logicalExpression nextExpressionParser (currentNode nextExpressionParser)
  | (tokenType (currentToken parser) == andOperation definedTypes) || (tokenType (currentToken parser) == orOperation definedTypes) =
    logicalExpression returnParser (Tree node (currentToken parser) BinaryOpNode (currentNode returnParser))
  | otherwise = parser{currentNode = node}
  where
    nextExpressionParser = compareExpression parser Empty
    returnParser = compareExpression (advanceParser parser) Empty

expression :: Parser -> Node -> Parser 
expression parser node 
  | node == Empty = expression nextExpressionParser (currentNode nextExpressionParser)
  | tokenType (currentToken parser) == assignOperation definedTypes =
    if tokenType (getToken node) == identifier definedTypes
    then expression returnParser (Tree node (currentToken parser) VarAssignNode (currentNode returnParser))
    else throwError(InvalidSyntaxError (file parser) "identifier" ("\"" ++ tokenType (getToken node) ++ "\"") (pos (getToken node)))
  | (tokenType (currentToken parser) == plusOperation definedTypes) || (tokenType (currentToken parser) == minusOperation definedTypes) =
    expression returnParser (Tree node (currentToken parser) BinaryOpNode (currentNode returnParser))
  | otherwise = parser{currentNode = node}
  where
    nextExpressionParser = logicalExpression parser Empty
    returnParser = logicalExpression (advanceParser parser) Empty

parse :: Parser -> Parser 
parse parser = 
    if tokenType (currentToken newParser) == "EOF"
    then newParser 
    else throwError(InvalidSyntaxError (file newParser) "operator" ("\"" ++ tokenType (currentToken newParser) ++ "\"") (pos (currentToken newParser)))
    where
      newParser = expression parser Empty

