module Interpreter where 
import Data.Maybe
import Data.Fixed
import Data.Map
import Prelude hiding (lookup)
import Parser
import Exception 
import Types

data Interpreter = Interpreter{
  intprFileName :: String,
  intEnv :: Environment,
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
  deriving Show 

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

addValue :: Value -> Value -> Value
addValue (Int a) (Int b) = Int( a + b)
addValue (Int a) (Float b) = Float( fromIntegral a + b)
addValue (Float a) (Int b) = Float( a + fromIntegral b)
addValue (Float a) (Float b) = Float(a + b)

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


isZero :: Value-> Bool
isZero (Int a) 
  | a == 0 = True
  | otherwise = False
isZero (Float a)
  | a == 0.0 = True
  | otherwise = False

setResult :: Number -> Interpreter -> Interpreter
setResult num inter =
  inter{currentResult = Just num}

visit :: Node -> Interpreter -> Interpreter
visit node intptr
  | nodeType node == "NumberNode" = visitNumberNode node intptr
  | nodeType node == "VarAccessNode" = visitVarAccessNode node intptr
  | nodeType node == "AssignNode" = visitVarAssignNode node intptr
  | nodeType node == "BinaryOpNode" = visitBinaryOpNode node intptr
  | nodeType node == "UnaryNode" = visitUnaryNode node intptr
  | otherwise = error "Internal error in interpreter"

visitNumberNode :: Node -> Interpreter -> Interpreter
visitNumberNode node inter =
  setResult (Number{numberValue = fromJust(val (token node)), numPos = Just (pos (token node))}) inter

visitVarAccessNode :: Node -> Interpreter -> Interpreter
visitVarAccessNode node intptr = 
  if isJust(value)
  then setResult Number{numberValue = fromJust(value), numPos = Just (pos (token node))} intptr
  else throwError(NotDefinedError (intprFileName intptr) ( "\"" ++ getVariableName(fromJust(val (token node))) ++ "\"") (pos (token node)))
  where 
    value = getEnvironmentValue (intEnv intptr) (getVariableName(fromJust(val (token node))))

visitVarAssignNode :: Node -> Interpreter -> Interpreter
visitVarAssignNode node intptr = 
  newEnv
  where
    variableNode = fromJust(leftNode node)
    valueNode = fromJust(currentResult (visit (fromJust(rightNode node)) intptr))
    varName = getVariableName(fromJust(val (token variableNode)))
    value = setResult valueNode intptr
    valueResult = fromJust(currentResult value)
    newEnv = value {intEnv = setEnvironmentValue (intEnv value) varName (numberValue valueResult)}

visitBinaryOpNode :: Node -> Interpreter -> Interpreter
visitBinaryOpNode node intptr
  | tokenType (token node) == plusOperation definedTypes = setResult (addNumber num1 num2) intptr
  | tokenType (token node) == minusOperation definedTypes = setResult (subNumber num1 num2) intptr
  | tokenType (token node) == multiplyOperation definedTypes = setResult (mulNumber num1 num2) intptr
  | tokenType (token node) == divisionOperation definedTypes = setResult (divNumber num1 numCheck) intptr
  | tokenType (token node) == modOperation definedTypes = setResult (modNumber num1 num2) intptr
  | tokenType (token node) == powerOperation definedTypes = setResult (powNumber num1 num2) intptr
  | tokenType (token node) == sqrootOperation definedTypes = setResult (sqrootNumber num2) intptr
  where 
    num1 = fromJust(currentResult (visit (fromJust(leftNode node)) intptr))
    num2 = fromJust(currentResult (visit (fromJust(rightNode node)) intptr))
    numCheck = 
      if isZero (numberValue num2)
      then throwError(DivisionByZeroError (intprFileName intptr)  (pos(token node)))
      else num2

visitUnaryNode :: Node -> Interpreter -> Interpreter
visitUnaryNode node intptr
  | tokenType (token node) == minusOperation definedTypes = setResult (mulNumber num1 Number{numberValue = Int(-1), numPos = Nothing}) intptr
  | otherwise = setResult num1 intptr
  where 
    num1 = fromJust(currentResult(visit (fromJust(leftNode node)) intptr))