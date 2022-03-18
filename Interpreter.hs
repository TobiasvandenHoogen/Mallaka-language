module Interpreter where 
import System.IO
import Data.Maybe
import Data.Fixed
import Data.Map hiding (null, map, foldr)
import Data.Bits 
import Data.List hiding (lookup, insert, delete)
import Prelude hiding (lookup)
import Parser
import Lexer
import Exception 
import Types

data Interpreter = Interpreter{
  intprFileName :: String,
  intEnv :: Environment,
  intError :: Error,
  currentResult :: Maybe Number
}
  deriving Show 

data Environment = Environment{
  lookupTable :: Map String Value,
  parent :: Maybe Environment
}
  deriving Show 

data Number = Number{
  numberValue :: Value,
  numPos :: Maybe Position
}
  deriving (Show, Eq)



getEnvironmentValue :: Environment -> String -> Maybe Value
getEnvironmentValue env name = 
  if isNothing returnValue && isJust(parent env)
  then getEnvironmentValue (fromJust(parent env))name
  else returnValue
  where 
    returnValue = lookup name (lookupTable env)

setEnvironmentValue :: Environment -> String -> Value -> Environment
setEnvironmentValue env key value = 
  env{lookupTable = newLookuptable}
  where 
    newLookuptable = insert key value (lookupTable env)

removeEnvironmentValue :: Environment -> String -> Environment
removeEnvironmentValue env key = 
  env{lookupTable = newLookuptable}
  where 
    newLookuptable = delete key (lookupTable env)
  
getVariableName :: Value -> String 
getVariableName (String a ) = a 

getBoolValue :: Value -> Interpreter -> Node -> (Interpreter ,Bool)
getBoolValue (Bool a) i n = (i, a)
getBoolValue a i n =  (i{intError = throwError (intError i) (ConditionError(intprFileName i) (pos (getToken n)))}, True) --throwError(ConditionError(intprFileName i) (pos (getToken n)))

addValue :: Value -> Value -> Value
addValue (Int a) (Int b) = Int( a + b)
addValue (Int a) (Float b) = Float( fromIntegral a + b)
addValue (Float a) (Int b) = Float( a + fromIntegral b)
addValue (Float a) (Float b) = Float(a + b)
addValue (String a) (String b) = String(a ++ b)

subValue :: Value -> Value -> Value
subValue (Int a) (Int b) = Int( a - b)
subValue (Int a) (Float b) = Float( fromIntegral a - b)
subValue (Float a) (Int b) = Float( a - fromIntegral b)
subValue (Float a) (Float b) = Float(a - b)

mulValue :: Value -> Value -> Value
mulValue (Int a) (Int b) = Int( a * b)
mulValue (Int a) (Float b) = Float( fromIntegral a * b)
mulValue (Float a) (Int b) = Float( a * fromIntegral b)
mulValue (Float a) (Float b) = Float(a * b)
mulValue (String a) (Int b) = String(concat (replicate b a))

divValue :: Value -> Value -> Value
divValue (Int a) (Int b) = Int( a `div` b)
divValue (Int a) (Float b) = Float( fromIntegral a / b)
divValue (Float a) (Int b) = Float( a / fromIntegral b)
divValue (Float a) (Float b) = Float(a / b)

modValue :: Value -> Value -> Value
modValue (Int a) (Int b) = Int( a `mod` b)
modValue (Int a) (Float b) = Float( mod' (fromIntegral a) b)
modValue (Float a) (Int b) = Float( mod' a (fromIntegral b))
modValue (Float a) (Float b) = Float( mod' a b)

powValue :: Value -> Value -> Value
powValue (Int a) (Int b) = Int( a ^ b)
powValue (Int a) (Float b) = Float( fromIntegral a ** b)
powValue (Float a) (Int b) = Float( a ** fromIntegral b)
powValue (Float a) (Float b) = Float(a ** b)

sqrootValue :: Value -> Value 
sqrootValue (Int a) = Int(round (sqrt (fromIntegral a)))
sqrootValue (Float a) = Float( sqrt a)

notValue :: Value -> Value 
notValue (Int a) = Int(complement a)
notValue (Float a) = Int(complement (round a))
notValue (Bool a) = Bool(not a)

andValue :: Value -> Value -> Value 
andValue (Int a) (Int b) = Int(a .&. b)
andValue (Float a) (Int b) = Int(round a .&. b)
andValue (Int a) (Float b) = Int(a .&. round b)
andValue (Float a) (Float b) = Int(round a .&. round b)
andValue (Bool a) (Bool b) = Bool(a && b)

orValue :: Value -> Value -> Value 
orValue (Int a) (Int b) = Int(a .|. b)
orValue (Float a) (Int b) = Int(round a .|. b)
orValue (Int a) (Float b) = Int(a .|. round b)
orValue (Float a) (Float b) = Int(round a .|. round b)
orValue (Bool a) (Bool b) = Bool(a || b)

eqComValue :: Value -> Value -> Value 
eqComValue (Int a) (Float b) = Bool( fromIntegral a == b)
eqComValue (Float a) (Int b) = Bool( a == fromIntegral b)
eqComValue (Bool a) (Int b) = Bool(fromEnum a == b)
eqComValue (Bool a) (Float b) = Bool(fromIntegral(fromEnum a) == b)
eqComValue (Int a) (Bool b) = Bool(a == fromEnum b)
eqComValue (Float a) (Bool b) = Bool(a == fromIntegral(fromEnum b))
eqComValue a b = Bool(a == b)

neqComValue :: Value -> Value -> Value 
neqComValue (Int a) (Float b) = Bool( fromIntegral a /= b)
neqComValue (Float a) (Int b) = Bool( a /= fromIntegral b)
neqComValue (Bool a) (Int b) = Bool(fromEnum a /= b)
neqComValue (Bool a) (Float b) = Bool(fromIntegral(fromEnum a) /= b)
neqComValue (Int a) (Bool b) = Bool(a /= fromEnum b)
neqComValue (Float a) (Bool b) = Bool(a /= fromIntegral(fromEnum b))
neqComValue a b = Bool(a /= b)

greaterComValue :: Value -> Value -> Value 
greaterComValue (Int a) (Float b) = Bool( fromIntegral a > b)
greaterComValue (Float a) (Int b) = Bool( a > fromIntegral b)
greaterComValue (Bool a) (Int b) = Bool(fromEnum a > b)
greaterComValue (Bool a) (Float b) = Bool(fromIntegral(fromEnum a) > b)
greaterComValue (Int a) (Bool b) = Bool(a > fromEnum b)
greaterComValue (Float a) (Bool b) = Bool(a > fromIntegral(fromEnum b))
greaterComValue a b = Bool(a > b)

lessComValue :: Value -> Value -> Value 
lessComValue (Int a) (Float b) = Bool( fromIntegral a < b)
lessComValue (Float a) (Int b) = Bool( a < fromIntegral b)
lessComValue (Bool a) (Int b) = Bool(fromEnum a < b)
lessComValue (Bool a) (Float b) = Bool(fromIntegral(fromEnum a) < b)
lessComValue (Int a) (Bool b) = Bool(a < fromEnum b)
lessComValue (Float a) (Bool b) = Bool(a < fromIntegral(fromEnum b))
lessComValue a b = Bool(a < b)

greaterEqComValue :: Value -> Value -> Value 
greaterEqComValue (Int a) (Float b) = Bool( fromIntegral a >= b)
greaterEqComValue (Float a) (Int b) = Bool( a >= fromIntegral b)
greaterEqComValue (Bool a) (Int b) = Bool(fromEnum a >= b)
greaterEqComValue (Bool a) (Float b) = Bool(fromIntegral(fromEnum a) >= b)
greaterEqComValue (Int a) (Bool b) = Bool(a >= fromEnum b)
greaterEqComValue (Float a) (Bool b) = Bool(a >= fromIntegral(fromEnum b))
greaterEqComValue a b = Bool(a >= b)

lessEqComValue :: Value -> Value -> Value 
lessEqComValue (Int a) (Float b) = Bool( fromIntegral a <= b)
lessEqComValue (Float a) (Int b) = Bool( a <= fromIntegral b)
lessEqComValue (Bool a) (Int b) = Bool(fromEnum a <= b)
lessEqComValue (Bool a) (Float b) = Bool(fromIntegral(fromEnum a) <= b)
lessEqComValue (Int a) (Bool b) = Bool(a <= fromEnum b)
lessEqComValue (Float a) (Bool b) = Bool(a <= fromIntegral(fromEnum b))
lessEqComValue a b = Bool(a <= b)

addNumber :: Number -> Number -> Number 
addNumber num1 num2 = 
  Number{numberValue = addValue(numberValue num1) (numberValue num2), numPos = Nothing}

subNumber :: Number -> Number -> Number 
subNumber num1 num2 = 
  Number{numberValue = subValue(numberValue num1) (numberValue num2), numPos = Nothing}

mulNumber :: Number -> Number -> Number 
mulNumber num1 num2 = 
  Number{numberValue = mulValue(numberValue num1) (numberValue num2), numPos = Nothing} 

divNumber :: Number -> Number -> Number 
divNumber num1 num2 = 
  Number{numberValue = divValue(numberValue num1) (numberValue num2), numPos = Nothing}

modNumber :: Number -> Number -> Number 
modNumber num1 num2 = 
  Number{numberValue = modValue(numberValue num1) (numberValue num2), numPos = Nothing}

powNumber :: Number -> Number -> Number 
powNumber num1 num2 = 
  Number{numberValue = powValue(numberValue num1) (numberValue num2), numPos = Nothing}

sqrootNumber :: Number -> Number 
sqrootNumber num1 = 
  Number{numberValue = sqrootValue(numberValue num1), numPos = Nothing }

notNumber :: Number -> Number 
notNumber num1 = 
  Number{numberValue = notValue(numberValue num1), numPos = Nothing }

andNumber :: Number -> Number -> Number 
andNumber num1 num2 = 
  Number{numberValue = andValue(numberValue num1) (numberValue num2), numPos = Nothing }

orNumber :: Number -> Number -> Number 
orNumber num1 num2 = 
  Number{numberValue = orValue(numberValue num1) (numberValue num2), numPos = Nothing }

eqNumber :: Number -> Number -> Number 
eqNumber num1 num2 = 
  Number{numberValue = eqComValue(numberValue num1) (numberValue num2), numPos = Nothing }

neqNumber :: Number -> Number -> Number 
neqNumber num1 num2 = 
  Number{numberValue = neqComValue(numberValue num1) (numberValue num2), numPos = Nothing }

greaterNumber :: Number -> Number -> Number 
greaterNumber num1 num2 = 
  Number{numberValue = greaterComValue(numberValue num1) (numberValue num2), numPos = Nothing }

lessNumber :: Number -> Number -> Number 
lessNumber num1 num2 = 
  Number{numberValue = lessComValue(numberValue num1) (numberValue num2), numPos = Nothing }

greaterEqNumber :: Number -> Number -> Number 
greaterEqNumber num1 num2 = 
  Number{numberValue = greaterEqComValue(numberValue num1) (numberValue num2), numPos = Nothing }

lessEqNumber :: Number -> Number -> Number 
lessEqNumber num1 num2 = 
  Number{numberValue = lessEqComValue(numberValue num1) (numberValue num2), numPos = Nothing }


isZero :: Value-> Bool
isZero (Int a) 
  | a == 0 = True
  | otherwise = False
isZero (Float a)
  | a == 0.0 = True
  | otherwise = False

zipMapNodeTree :: (Node -> Interpreter -> Interpreter) -> [c] -> Node -> Interpreter -> [(c, Number)]
zipMapNodeTree f [] b c = []
zipMapNodeTree f a e@(Empty) c = [(head a,  fromJust(currentResult (f e c)))]
zipMapNodeTree f a l@(Leaf _ _)  c = [(head a, fromJust(currentResult (f l c)))]
zipMapNodeTree f a b@(Branch _ _ r1) c = [(head a, fromJust(currentResult (f b c)))] ++ zipMapNodeTree f (tail a) r1 c  
zipMapNodeTree f a tr@(Tree l1 _ _ r1) c = [(head a, fromJust(currentResult (f l1 c)))] ++ zipMapNodeTree f (tail a) r1 c 

setResult :: Number -> Interpreter -> Interpreter
setResult num inter =
  inter{currentResult = Just num}

visit :: Node -> Interpreter -> Interpreter
visit node intptr
  | hasOccurred (intError intptr) = intptr 
  | typ == NumberNode = visitNumberNode node intptr
  | typ == VarAccessNode = visitVarAccessNode node intptr
  | typ == VarAssignNode = visitVarAssignNode node intptr
  | typ == BinaryOpNode = visitBinaryOpNode node intptr
  | typ == UnaryNode = visitUnaryNode node intptr
  | typ == IfNode = visitIfNode node intptr
  | typ == LoopNode = visitLoopNode node intptr
  | typ == UntilNode = visitUntilNode node intptr
  | typ == FunctionAssignNode = visitCreateFunctionNode node intptr
  | typ == FunctionRunNode = visitRunFunctionNode node intptr
  | otherwise = error (show node)
  where 
    typ = getNodeType node



visitNumberNode :: Node -> Interpreter -> Interpreter
visitNumberNode node intptr = case val tok of 
  (Just a) -> setResult (Number{numberValue = fromJust(val tok), numPos = Just (pos tok)}) intptr
  Nothing -> intptr
  where
    tok = getToken node

visitVarAccessNode :: Node -> Interpreter -> Interpreter
visitVarAccessNode node intptr = 
  if isJust(value)
  then setResult Number{numberValue = fromJust(value), numPos = Just (pos tok)} intptr
  else intptr{intError = throwError (intError intptr) (NotDefinedError (intprFileName intptr) ( "\"" ++ getVariableName(fromJust(val tok)) ++ "\"") (pos tok))}
  where 
    tok = getToken node
    value = getEnvironmentValue (intEnv intptr) (getVariableName(fromJust(val tok)))

visitVarAssignNode :: Node -> Interpreter -> Interpreter
visitVarAssignNode node@(Tree left tok _ right) intptr 
  | hasOccurred (intError visitValue) = intptr{intError = intError visitValue} 
  | otherwise = newEnv
  where
    visitValue = visit right intptr
    valueNode = fromJust(currentResult visitValue)
    varName = getVariableName(fromJust(val (getToken left)))
    value = setResult valueNode intptr
    valueResult = fromJust(currentResult value)
    newEnv = value {intEnv = setEnvironmentValue (intEnv value) varName (numberValue valueResult)}

visitBinaryOpNode :: Node -> Interpreter -> Interpreter
visitBinaryOpNode node@(Tree left tok _ right) intptr
  | hasOccurred (intError visitRight) = intptr{intError = (intError visitRight)}
  | tokenType tok == sqrootOperation definedTypes =  setResult (sqrootNumber num2) intptr
  | hasOccurred (intError visitLeft) = intptr{intError = (intError visitLeft)}
  | tokenType tok == plusOperation definedTypes = setResult (addNumber num1 num2) intptr
  | tokenType tok == minusOperation definedTypes = setResult (subNumber num1 num2) intptr
  | tokenType tok == multiplyOperation definedTypes = setResult (mulNumber num1 num2) intptr
  | tokenType tok == divisionOperation definedTypes = numCheck
  | tokenType tok == modOperation definedTypes = setResult (modNumber num1 num2) intptr
  | tokenType tok == powerOperation definedTypes = setResult (powNumber num1 num2) intptr
  | tokenType tok == andOperation definedTypes = setResult (andNumber num1 num2) intptr
  | tokenType tok == orOperation definedTypes = setResult (orNumber num1 num2) intptr
  | tokenType tok == equalOperation definedTypes = setResult (eqNumber num1 num2) intptr
  | tokenType tok == notEqualOperation definedTypes = setResult (neqNumber num1 num2) intptr
  | tokenType tok == greaterOperation definedTypes = setResult (greaterNumber num1 num2) intptr
  | tokenType tok == lessOperation definedTypes = setResult (lessNumber num1 num2) intptr
  | tokenType tok == greaterEqOperation definedTypes = setResult (greaterEqNumber num1 num2) intptr
  | tokenType tok == lessEqOperation definedTypes = setResult (lessEqNumber num1 num2) intptr
  where 
    visitLeft = visit left intptr
    visitRight = visit right intptr
    num1 = fromJust(currentResult (visitLeft))
    num2 = fromJust(currentResult (visitRight))
    numCheck = 
      if isZero (numberValue num2)
      then intptr{intError = throwError (intError intptr) (DivisionByZeroError (intprFileName intptr) (pos tok))}
      else setResult (divNumber num1 num2) intptr

visitUnaryNode :: Node -> Interpreter -> Interpreter
visitUnaryNode node@(Branch _ _ right) intptr
  | hasOccurred (intError visitValue) = intptr{intError = intError visitValue} 
  | tokenType tok == minusOperation definedTypes = setResult (mulNumber num1 Number{numberValue = Int(-1), numPos = Nothing}) intptr
  | tokenType tok == notOperation definedTypes = setResult (notNumber num1) intptr
  | otherwise = setResult num1 intptr
  where 
    visitValue = visit right intptr
    num1 = fromJust(currentResult visitValue)
    tok = getToken node

visitIfNode :: Node -> Interpreter -> Interpreter
visitIfNode node@(Tree left tok _ right) intptr
    | tokenType tok == ifOperation definedTypes ||
      tokenType tok == elseIfOperation definedTypes = 
      visitIfConditionNode node intptr 
    | hasOccurred (intError elseStatement) = intptr{intError = intError elseStatement}
    | tokenType tok == elseOperation definedTypes =
      setResult elseResult intptr 
    | otherwise = intptr
    where 
      elseStatement = visit left intptr 
      elseResult = fromJust(currentResult elseStatement)
visitIfNode node _ = error (show node)

visitIfConditionNode :: Node -> Interpreter -> Interpreter 
visitIfConditionNode node@(Tree left@(Tree leftLeft _ _ leftRight) tok _ right) intptr
 | hasOccurred (intError conditionBranch) = intptr{intError = intError conditionBranch}
 | hasOccurred (intError errIntptr) = errIntptr 
 | conditionResult = setResult ifResult intptr
 | otherwise = goToNextCondition
 where 
   conditionBranch = visit leftLeft intptr
   conditionVisit = getBoolValue (numberValue(fromMaybe Number{} (currentResult conditionBranch))) intptr leftLeft
   errIntptr = fst conditionVisit
   conditionResult = snd conditionVisit 
   ifStatement = visit leftRight intptr
   ifResult = fromJust(currentResult ifStatement)
   goToNextCondition = 
        if right == Empty
        then intptr
        else visitIfNode right intptr

visitLoopNode :: Node -> Interpreter -> Interpreter
visitLoopNode node@(Tree left@(Tree leftLeft _ _ leftRight) tok typ right@(Tree rightLeft _ _ rightRight)) intptr 
  | hasOccurred (intError withVisit) = intptr{intError = intError withVisit}
  | otherwise = runLoopNode fromValue toValue withValue statementNode intptr
  where 
    fromVisit = visit leftLeft intptr
    toVisit = visit leftRight fromVisit
    withVisit = visit rightLeft toVisit
    fromValue = fromJust(currentResult ( fromVisit ) )
    toValue = fromJust(currentResult (toVisit ) )
    withValue = fromJust(currentResult (withVisit ) )
    statementNode = rightRight
visitLoopNode node@(Branch _ _ right) intptr = visit right intptr


runLoopNode :: Number -> Number -> Number -> Node -> Interpreter -> Interpreter
runLoopNode fromValue toValue withValue statementNode intptr 
  | numberValue fromValue == numberValue toValue  = intptr
  | otherwise = runLoopNode newIndex toValue withValue statementNode nextIteration
  where 
    newIndex = Number{numberValue = addValue (numberValue fromValue) (numberValue withValue), numPos = Nothing}
    nextIteration = visit statementNode intptr


visitUntilNode :: Node -> Interpreter -> Interpreter
visitUntilNode node@(Tree left _ _ right) intptr 
  | hasOccurred (intError conditionVisit) = intptr{intError = intError conditionVisit}
  | snd (getBoolValue (numberValue conditionResult) intptr left) = intptr
  | otherwise = visitUntilNode node nextIteration
  where 
    conditionVisit = visit left intptr
    conditionResult = fromJust(currentResult (conditionVisit))
    nextIteration = visit right intptr

visitCreateFunctionNode :: Node -> Interpreter -> Interpreter
visitCreateFunctionNode node@(Tree l1 _ _ (Tree r1 _ _ r2)) intptr = newEnv
 where 
   funcName = getVariableName(fromJust(val (getToken l1)))
   param = visitCreateParameterNode r1
   function = Function{parameters = param, statement = r2}
   newEnv = intptr {intEnv = setEnvironmentValue (intEnv intptr) funcName (Func function)}


visitCreateParameterNode :: Node -> [String]
visitCreateParameterNode node@(Branch tok typ r1) = [checkString(fromJust(val(tok)))] ++ visitCreateParameterNode r1
visitCreateParameterNode Empty = []

checkString :: Value -> String
checkString (String a) = a 

visitRunFunctionNode :: Node -> Interpreter -> Interpreter
visitRunFunctionNode node@(Tree l1 _ _ r1) intptr = case getEnvironmentValue (intEnv intptr) funcName of 
  Just (Func a) -> visitFunctionStatement (statement a) intptr (visitRunParameters r1 (parameters a) intptr)
  Nothing -> intptr{intError = throwError (intError intptr) (NotDefinedError (intprFileName intptr) ( "\"" ++ funcName ++ "\"") (pos (getToken l1)))}
  _ -> intptr{intError = throwError (intError intptr) (InvalidSyntaxError (intprFileName intptr) "function" ( "\"" ++ tokenType (getToken l1) ++ "\"")  (pos (getToken l1)))}
  where
    funcName = getVariableName(fromJust(val (getToken l1)))

visitRunParameters :: Node -> [String] -> Interpreter -> Interpreter
visitRunParameters parVal parNam intptr = case (getNodeLength parVal) == (length parNam) of 
  True ->  setParameterValue intptr (zipMapNodeTree visit parNam parVal intptr)
  False -> intptr{intError = throwError (intError intptr) (InvalidNumberOfArguments (intprFileName intptr) (length parNam) (getNodeLength parVal) (pos (getToken parVal)))}

setParameterValue :: Interpreter -> [(String, Number)] -> Interpreter
setParameterValue intptr [] = intptr
setParameterValue intptr [(name, val)] = intptr {intEnv = setEnvironmentValue (intEnv intptr) name (numberValue val)} 
setParameterValue intptr (x@(name, val):xs)  = setParameterValue (intptr {intEnv = setEnvironmentValue (intEnv intptr) name (numberValue val)}) xs

visitFunctionStatement :: Node -> Interpreter -> Interpreter -> Interpreter 
visitFunctionStatement stat intptr funcIntptr = case currentResult newIntptr of 
  (Just a) -> setResult (fromJust (currentResult newIntptr)) intptr
  Nothing -> newIntptr
  where 
    newIntptr = visit stat funcIntptr

runResult :: Interpreter -> String -> Interpreter
runResult inter input 
  | null input = inter{intError = (Error{hasOccurred = False, errorMessage = []})}
  | otherwise = newInter
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
      lexerError = Error{hasOccurred = False, errorMessage = []},
      tokenList = []}
    tokenLexer = createTokens lexer 
    parser = Parser {
      tokens = tokenList tokenLexer,
      tokenIndex = 1,
      currentToken = head (tokenList tokenLexer),
      currentNode = Empty,
      file = fileName tokenLexer,
      errorParser = lexerError tokenLexer}
    nodeTree = parse parser 
    checkInter = inter{intError = errorParser nodeTree}
    newInter = visit (currentNode nodeTree) checkInter

runInterpreter :: Interpreter -> IO ()
runInterpreter inter = do 
  hSetBuffering stdout NoBuffering
  putStr ">> " 
  input <- getLine

  let result = runResult inter input
  checkResult result 
  runInterpreter result{currentResult = Nothing}

checkResult :: Interpreter -> IO ()
checkResult inter = do 
  if  hasOccurred (intError inter)
  then putStrLn (concat (intersperse "\n" (errorMessage (intError inter)))) 
  else printResult (currentResult inter)

printResult :: Maybe Number -> IO ()
printResult a 
  | isJust a = putStrLn( show (numberValue (fromJust a)))
  | otherwise = putStr("")
      

