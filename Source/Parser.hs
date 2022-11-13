{- 
Module      : Parser
Description : The parser of the interpreter which converts the tokens into a parse tree.
Maintainer  : Tobias van den Hoogen  
-}

{-# LANGUAGE DerivingStrategies #-}
module Source.Parser(module Source.Parser) where 

import Data.Maybe ()
import Source.Exception 
import Source.Types 

----------Parser

data Parser = Parser{
  tokens :: [Token],
  tokenIndex :: Int,
  currentToken :: Token,
  currentNode :: Node,
  file :: String,
  errorParser :: Error }
  deriving stock Show 

advanceParser :: Parser -> Parser
advanceParser parser 
  | hasOccurred  (errorParser parser) = parser  
  | otherwise =  
    let newToken = 
            if tokenIndex parser < length (tokens parser)
            then head (take (tokenIndex parser) (drop (tokenIndex parser) (tokens parser) ))
            else currentToken parser
        result = Parser {tokens = tokens parser,
                        tokenIndex = tokenIndex parser + 1,
                        currentToken = newToken,
                        currentNode = currentNode parser,
                        file = file parser,
                        errorParser = errorParser parser}
    in result 

statementExpression :: Parser -> Bool -> Parser 
statementExpression parser isRecursing 
  | tokenType (currentToken parser) == openStatement definedTypes && tokenType (currentToken nextCharacter) == closeStatement definedTypes =
    advanceParser nextCharacter { currentNode = Leaf (Token{tokenType = nullType definedTypes, tokenVal = Nothing, tokenPos = tokenPos (currentToken parser) }) ValueNode}
  | tokenType (currentToken parser) == openStatement definedTypes || isRecursing =  
    case tokenType (currentToken parsedStatement) of 
      ";" -> case tokenType (currentToken checkNextAfterStatement) of
        "<-" -> advanceParser checkNextAfterStatement
        _ -> nextStatement {currentNode = Tree (currentNode parsedStatement) (currentToken parsedStatement) SeparatorNode (currentNode nextStatement)}
      _ -> parsedStatement {errorParser = throwError (errorParser parser) (UnexpectedEndOfFile (file parsedStatement) (tokenPos (currentToken parsedStatement)))}
  | otherwise = advanceParser parsedStatement {errorParser = throwError (errorParser parsedStatement) (InvalidSyntaxError (file parser) (openStatement definedTypes) ("\"" ++ tokenType (currentToken parser) ++ "\"") (tokenPos (currentToken parser)))}
  where 
    nextCharacter = advanceParser parser
    parsedStatement = expression (advanceParser parser) Empty
    checkNextAfterStatement = advanceParser parsedStatement
    nextStatement = statementExpression parsedStatement True 

listExpression :: Parser -> Parser 
listExpression parser 
  | tokenType (currentToken parser) == closeList definedTypes = advanceParser parser
  | tokenType (currentToken getElement) == separatorParameter definedTypes = nextElement{currentNode = elementTree}
  | tokenType (currentToken getElement) == closeList definedTypes = (advanceParser getElement){currentNode = endOfElementTree}
  | otherwise = advanceParser parser {errorParser = throwError (errorParser parser) (InvalidSyntaxError (file parser) (closeList definedTypes) ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (tokenPos (currentToken parser)))}
  where 
    getElement = atom parser
    elementTree = Tree (currentNode getElement) (currentToken getElement) NoType (currentNode nextElement)
    nextElement = listExpression (advanceParser getElement)
    endOfElementTree = Tree (currentNode getElement) (currentToken getElement)  NoType Empty

  

ifExpression :: Parser -> Parser 
ifExpression parser 
  | tokenType (currentToken parser) == ifOperation definedTypes ||
    tokenType (currentToken parser) == elseIfOperation definedTypes  = otherStatements {currentNode = ifTree}
  | tokenType (currentToken parser) == elseOperation definedTypes = elseStatement {currentNode = elseNode}
  | otherwise = parser {currentNode = Empty}
  where 
    condition = expression (advanceParser parser) Empty
    ifStatement = statementExpression condition False
    otherStatements = ifExpression ifStatement
    elseStatement = statementExpression (advanceParser parser) False
    ifTree = Tree ifNode (currentToken parser) IfNode (currentNode otherStatements)
    ifNode = Tree (currentNode condition) (currentToken parser) NoType (currentNode ifStatement)
    elseNode = Tree (currentNode elseStatement) (currentToken parser) NoType Empty

loopExpression :: Parser -> Parser 
loopExpression parser 
  | tokenType (currentToken parser) == loopOperation definedTypes = statementBranch{currentNode = loopBranch}
  | tokenType (currentToken parser) == fromLoopOperation definedTypes ||
    tokenType (currentToken parser) == toLoopOperation definedTypes ||
    tokenType (currentToken parser) == withLoopOperation definedTypes = 
      loopAttribute { currentNode = Branch (currentToken parser) LoopNode (currentNode loopAttribute)}
  | otherwise = advanceParser parser {errorParser = throwError (errorParser parser) (InvalidSyntaxError (file parser) "loop keyword" ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (tokenPos (currentToken parser)))}
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
    statementBranch = statementExpression withBranch False

untilExpression :: Parser -> Parser 
untilExpression parser = statementBranch{currentNode = untilBranch} 
  where 
    untilBranch = Tree (currentNode conditionBranch) (currentToken parser) UntilNode (currentNode statementBranch)
    conditionBranch = expression (advanceParser parser) Empty
    statementBranch = statementExpression conditionBranch False

runFunctionExpression :: Parser -> Parser 
runFunctionExpression parser 
  | tokenType (currentToken parser) == runFunction definedTypes =
    if tokenType (currentToken (advanceParser parser)) == identifier definedTypes
    then newParser{currentNode = functionTree}
    else advanceParser parser {errorParser = throwError (errorParser parser) (InvalidSyntaxError (file parser) "identifier" ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (tokenPos (currentToken parser)))}
  | otherwise = advanceParser parser {errorParser = throwError (errorParser parser) (InvalidSyntaxError (file parser) "function keyword" ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (tokenPos (currentToken parser)))}
  where 
    identLeaf = Leaf (currentToken (advanceParser parser)) NoType 
    functionTree = Tree identLeaf (currentToken parser) FunctionRunNode (currentNode newParser) 
    newParser = runParameterExpression (advanceParser (advanceParser parser))

runParameterExpression :: Parser -> Parser 
runParameterExpression parser 
  | tokenType (currentToken parser) == openParameter definedTypes && 
    tokenType (currentToken (advanceParser parser)) == closeParameter definedTypes = (advanceParser (advanceParser parser)){currentNode = Empty}
  | tokenType (currentToken parser) == openParameter definedTypes  = parameterParser
  | otherwise = advanceParser parser {errorParser = throwError (errorParser parser)(InvalidSyntaxError (file parser) "function keyword" ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (tokenPos (currentToken parser)))}
  where
    parameterParser = runParameters(advanceParser parser)

runParameters :: Parser -> Parser 
runParameters parser 
  | tokenType (currentToken parser) == closeParameter definedTypes = advanceParser parser
  | tokenType (currentToken getParameter) == separatorParameter definedTypes = endParameter{currentNode = parameterTree}
  | tokenType (currentToken getParameter) == closeParameter definedTypes = (advanceParser getParameter){currentNode = endParameterTree}
  | otherwise = advanceParser parser {errorParser = throwError (errorParser parser) (InvalidParameterName (file parser) ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (tokenPos (currentToken parser)))}
  where 
    getParameter = atom parser
    parameterTree = Tree (currentNode getParameter) (currentToken getParameter) NoType (currentNode nextParameter)
    endParameterTree = Tree (currentNode getParameter) (currentToken getParameter) NoType Empty 
    nextParameter = runParameters (advanceParser getParameter)
    endParameter = nextParameter
    
createFunctionExpression :: Parser -> Parser 
createFunctionExpression parser 
  | tokenType (currentToken parser) == function definedTypes = 
    if tokenType (currentToken (advanceParser parser)) == identifier definedTypes
    then newParser{currentNode = functionTree}
    else advanceParser parser {errorParser = throwError (errorParser parser) (InvalidSyntaxError (file parser) "identifier" ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (tokenPos (currentToken parser)))}
  | otherwise = advanceParser parser {errorParser = throwError (errorParser parser) (InvalidSyntaxError (file parser) "function keyword" ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (tokenPos (currentToken parser)))}
  where 
    varLeaf = Leaf (currentToken (advanceParser parser)) NoType
    functionTree = Tree varLeaf (currentToken parser) FunctionAssignNode (currentNode newParser) 
    newParser = createParameterExpression (advanceParser (advanceParser parser))
  
createParameterExpression :: Parser -> Parser 
createParameterExpression parser 
  | tokenType (currentToken parser) == openParameter definedTypes = statementParser{currentNode = inputBranch} 
  | otherwise = advanceParser parser {errorParser = throwError (errorParser parser) (InvalidSyntaxError (file parser) "function keyword" ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (tokenPos (currentToken parser)))}
  where
    parameterParser = registerParameters(advanceParser parser)
    statementParser = statementExpression parameterParser False
    inputBranch = Tree (currentNode parameterParser) (currentToken parser) NoType (currentNode statementParser)


registerParameters :: Parser -> Parser 
registerParameters parser 
  | tokenType (currentToken parser) == closeParameter definedTypes = advanceParser parser
  | tokenType (currentToken parser) == identifier definedTypes = case  tokenType (currentToken checkSeperator) of 
    "," -> endParameter{currentNode = registerParameter}
    "}" -> (advanceParser checkSeperator){currentNode = Branch (currentToken parser) NoType Empty}
    _ -> advanceParser parser {errorParser = throwError (errorParser parser) (InvalidSyntaxError (file parser) ", or }" ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (tokenPos (currentToken parser)))}
  | otherwise = advanceParser parser {errorParser = throwError (errorParser parser) (InvalidParameterName (file parser) ( "\"" ++ tokenType (currentToken parser) ++ "\"")  (tokenPos (currentToken parser)))}
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
    (tokenType (currentToken parser) == falseBool definedTypes) ||
    (tokenType (currentToken parser) == stringType definedTypes) = 
      advanceParser parser { currentNode = Leaf (currentToken parser) ValueNode}
  | tokenType (currentToken parser) == identifier definedTypes =  
    advanceParser parser { currentNode = Leaf (currentToken parser) VarAccessNode}
  | tokenType (currentToken parser) == leftParent definedTypes = 
    if tokenType (currentToken expressionParser) == rightParent definedTypes
    then advanceParser expressionParser 
    else advanceParser parser {errorParser = throwError (errorParser parser) (InvalidSyntaxError (file parser) ")" ("\"" ++ tokenType (currentToken expressionParser) ++ "\"") (tokenPos (currentToken expressionParser)))}
  | tokenType (currentToken parser) == openList definedTypes = 
    createList{currentNode = Branch (currentToken parser) ListNode (currentNode createList)} 
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
  | tokenType (currentToken parser) == importFunction definedTypes =
    getPrintArgument {currentNode = Branch (currentToken parser) ImportNode (currentNode getPrintArgument)}
  | tokenType (currentToken parser) == printFunction definedTypes =
    getPrintArgument {currentNode = Branch (currentToken parser) PrintNode (currentNode getPrintArgument)}
  | otherwise = 
    advanceParser parser {errorParser = throwError (errorParser parser) (InvalidSyntaxError (file parser) "value" ("\"" ++ tokenType (currentToken parser) ++ "\"") (tokenPos (currentToken parser)))}
  where 
    nextParser = advanceParser parser
    createList = listExpression nextParser
    expressionParser = expression (advanceParser parser) Empty
    getPrintArgument = atom (advanceParser parser)

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
  | tokenType (currentToken parser) == indexOperation definedTypes  =
      term returnParser (Tree node (currentToken parser) BinaryOpNode (currentNode returnParser))
  | tokenType (currentToken parser) == sqrootOperation definedTypes  =
    exponential returnParser (Tree Empty (currentToken parser)  BinaryOpNode (currentNode returnParser))
  | node == Empty = exponential nextExpressionParser (currentNode nextExpressionParser)
  | tokenType (currentToken parser) == powerOperation definedTypes  =
      exponential returnParser (Tree node (currentToken parser) BinaryOpNode (currentNode returnParser))
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
    case tokenType (getToken node) of 
    "identifier" -> expression returnParser (Tree node (currentToken parser) VarAssignNode (currentNode returnParser))
    "$" -> expression returnParser (Tree node (currentToken parser) IndexAssignNode (currentNode returnParser))
    _ -> advanceParser parser {errorParser = throwError (errorParser parser) (InvalidSyntaxError (file parser) "identifier" ("\"" ++ tokenType (getToken node) ++ "\"") (tokenPos (getToken node)))}
   
  | (tokenType (currentToken parser) == plusOperation definedTypes) || (tokenType (currentToken parser) == minusOperation definedTypes) =
    expression returnParser (Tree node (currentToken parser) BinaryOpNode (currentNode returnParser))
  | otherwise = parser{currentNode = node}
  where
    nextExpressionParser = logicalExpression parser Empty
    returnParser = expression (advanceParser parser) Empty


parse :: Parser -> Parser 
parse parser 
  | hasOccurred (errorParser parser) = parser 
  | tokenType (currentToken newParser) == ";" = 
    if tokenType (currentToken checkNextCharacter) == "EOF"
    then newParser 
    else nextStatement {currentNode = Tree (currentNode newParser) (currentToken newParser) SeparatorNode (currentNode nextStatement)}
  | tokenType (currentToken newParser) == "EOF" = newParser {errorParser = throwError (errorParser parser) (UnexpectedEndOfFile (file newParser) (tokenPos (currentToken newParser)))}
  | otherwise = advanceParser parser {errorParser = throwError (errorParser parser) (InvalidSyntaxError (file newParser) "operator" ("\"" ++ tokenType (currentToken newParser) ++ "\"") (tokenPos (currentToken newParser)))}
    where
      newParser = expression parser Empty
      checkNextCharacter = advanceParser newParser 
      nextStatement = parse (advanceParser newParser) 

