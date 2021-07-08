module Interpreter where 
import Data.Maybe
import Data.Fixed
import Data.Map
import Data.Bits 
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
  | tokenType (token node) == andOperation definedTypes = setResult (andNumber num1 num2) intptr
  | tokenType (token node) == orOperation definedTypes = setResult (orNumber num1 num2) intptr
  | tokenType (token node) == equalOperation definedTypes = setResult (eqNumber num1 num2) intptr
  | tokenType (token node) == notEqualOperation definedTypes = setResult (neqNumber num1 num2) intptr
  | tokenType (token node) == greaterOperation definedTypes = setResult (greaterNumber num1 num2) intptr
  | tokenType (token node) == lessOperation definedTypes = setResult (lessNumber num1 num2) intptr
  | tokenType (token node) == greaterEqOperation definedTypes = setResult (greaterEqNumber num1 num2) intptr
  | tokenType (token node) == lessEqOperation definedTypes = setResult (lessEqNumber num1 num2) intptr
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
  | tokenType (token node) == notOperation definedTypes = setResult (notNumber num1) intptr
  | otherwise = setResult num1 intptr
  where 
    num1 = fromJust(currentResult(visit (fromJust(leftNode node)) intptr))