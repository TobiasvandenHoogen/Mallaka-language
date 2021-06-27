module Interpreter where 
import Data.Maybe
import Parser
import Exception 
import Types

newtype Interpreter = Interpreter{
  intprFileName :: String
}

data Number = Number{
  numberValue :: Value,
  numPos :: Maybe Position
}
  deriving Show 

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

isZero :: Value-> Bool
isZero (Int a) 
  | a == 0 = True
  | otherwise = False
isZero (Float a)
  | a == 0.0 = True
  | otherwise = False


visit :: Node -> Interpreter -> Number 
visit node intptr
  | nodeType node == "NumberNode" = visitNumberNode node intptr
  | nodeType node == "BinaryOpNode" = visitBinaryOpNode node intptr
  | nodeType node == "UnaryNode" = visitUnaryNode node intptr
  | otherwise = error "Internal error in interpreter"

visitNumberNode :: Node -> Interpreter -> Number 
visitNumberNode node intptr =
  Number{numberValue = fromJust(val (token node)), numPos = Just (pos (token node))}

visitBinaryOpNode :: Node -> Interpreter -> Number
visitBinaryOpNode node intptr
  | tokenType (token node) == plusOperation definedTypes = addNumber num1 num2
  | tokenType (token node) == minusOperation definedTypes = subNumber num1 num2
  | tokenType (token node) == multiplyOperation definedTypes = mulNumber num1 num2
  | tokenType (token node) == divisionOperation definedTypes = divNumber num1 numCheck 
  where 
    num1 = visit (fromJust(leftNode node)) intptr
    num2 = visit (fromJust(rightNode node)) intptr
    numCheck = 
      if isZero (numberValue num2)
      then throwError(DivisionByZeroError (intprFileName intptr)  (pos(token node)))
      else num2

visitUnaryNode :: Node -> Interpreter -> Number
visitUnaryNode node intptr
  | tokenType (token node) == minusOperation definedTypes = mulNumber num1 Number{numberValue = Int(-1), numPos = Nothing}
  | otherwise = num1
  where 
    num1 = visit (fromJust(leftNode node)) intptr