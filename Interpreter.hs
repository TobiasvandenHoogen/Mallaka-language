module Interpreter where 
import System.IO
import Data.Maybe
import Data.Fixed
import Control.Monad
import Data.Map hiding (null, map, foldr, drop, take, fold)
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
  currentResult :: Maybe Number,
  printResultList :: [Maybe Number]
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
getBoolValue a i n =  (i{intError = throwError (intError i) (ConditionError(intprFileName i) (pos (getToken n)))}, True) 

addNumber :: Number -> Number -> Interpreter -> Interpreter
addNumber (Number (Int val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Int(val1 + val2), numPos = pos1}) intptr
addNumber (Number (Int val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Float(fromIntegral val1 + val2), numPos = pos1}) intptr
addNumber (Number (Float val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Float(val1 + fromIntegral val2), numPos = pos1}) intptr
addNumber (Number (Float val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Float(val1 + val2), numPos = pos1}) intptr
addNumber (Number (String val1) pos1) (Number val2 pos2) intptr = setResult (Number {numberValue = String(val1 ++ (show val2)), numPos = pos1}) intptr
addNumber (Number (List val1) pos1) (Number val2 pos2) intptr = setResult (Number {numberValue = List(val1 ++ [val2]), numPos = pos1}) intptr
addNumber (Number val1 pos1) (Number val2 pos2) intptr = intptr{intError = throwError (intError intptr) (InvalidOperation (intprFileName intptr) (plusOperation definedTypes) (printValueType val1) (printValueType val2) (fromJust pos1))}

subNumber :: Number -> Number -> Interpreter -> Interpreter
subNumber (Number (Int val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Int(val1 - val2), numPos = pos1}) intptr
subNumber (Number (Int val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Float(fromIntegral val1 - val2), numPos = pos1}) intptr
subNumber (Number (Float val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Float(val1 - fromIntegral val2), numPos = pos1}) intptr
subNumber (Number (Float val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Float(val1 - val2), numPos = pos1}) intptr
subNumber (Number (List val1) pos1) (Number (Int val2) pos2) intptr 
  | val2 < 0 && (length val1 + val2) >= 0 = setResult (Number {numberValue = List(take (length val1 + val2) val1 ++ drop (length val1 + val2 + 1) val1), numPos = pos1}) intptr
  | val2 >= 0 && length val1 > val2 = setResult (Number {numberValue = List(take val2 val1 ++ drop (val2 + 1) val1) , numPos = pos1}) intptr 
  | otherwise = intptr{intError = throwError (intError intptr) (OutOfBoundsIndex (intprFileName intptr) (show val2) (fromJust pos1))}
subNumber  (Number val1 pos1) (Number val2 pos2) intptr = intptr{intError = throwError (intError intptr) (InvalidOperation (intprFileName intptr) (minusOperation definedTypes) (printValueType val1) (printValueType val2) (fromJust pos1))}

mulNumber :: Number -> Number -> Interpreter -> Interpreter
mulNumber (Number (Int val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Int(val1 * val2), numPos = pos1}) intptr
mulNumber (Number (Int val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Float(fromIntegral val1 * val2), numPos = pos1}) intptr
mulNumber (Number (Float val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Float(val1 * fromIntegral val2), numPos = pos1}) intptr
mulNumber (Number (Float val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Float(val1 * val2), numPos = pos1}) intptr
mulNumber (Number (String val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = String(concat (replicate val2 val1)), numPos = pos1}) intptr
mulNumber (Number (List val1) pos1) (Number (List val2) pos2) intptr = setResult (Number {numberValue = List(val1 ++ val2), numPos = pos1}) intptr
mulNumber (Number val1 pos1) (Number val2 pos2) intptr = intptr{intError = throwError (intError intptr) (InvalidOperation (intprFileName intptr) (multiplyOperation definedTypes) (printValueType val1) (printValueType val2) (fromJust pos1))}

divNumber :: Number -> Number -> Interpreter -> Interpreter
divNumber (Number (Int val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Int(val1 `div` val2), numPos = pos1}) intptr
divNumber (Number (Int val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Float(fromIntegral val1 / val2), numPos = pos1}) intptr
divNumber (Number (Float val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Float(val1 / fromIntegral val2), numPos = pos1}) intptr
divNumber (Number (Float val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Float(val1 / val2), numPos = pos1}) intptr
divNumber  (Number val1 pos1) (Number val2 pos2) intptr = intptr{intError = throwError (intError intptr) (InvalidOperation (intprFileName intptr) (divisionOperation definedTypes) (printValueType val1) (printValueType val2) (fromJust pos1))}

modNumber :: Number -> Number -> Interpreter -> Interpreter
modNumber (Number (Int val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Int(val1 `mod` val2), numPos = pos1}) intptr
modNumber (Number (Int val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Float( mod' (fromIntegral val1) val2), numPos = pos1}) intptr
modNumber (Number (Float val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Float( mod' val1 (fromIntegral val2)), numPos = pos1}) intptr
modNumber (Number (Float val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Float( mod' val1 val2), numPos = pos1}) intptr
modNumber  (Number val1 pos1) (Number val2 pos2) intptr = intptr{intError = throwError (intError intptr) (InvalidOperation (intprFileName intptr) (modOperation definedTypes) (printValueType val1) (printValueType val2) (fromJust pos1))}

powValue :: Value -> Value -> Value
powValue (Int a) (Int b) = Int( a ^ b)
powValue (Int a) (Float b) = Float( fromIntegral a ** b)
powValue (Float a) (Int b) = Float( a ** fromIntegral b)
powValue (Float a) (Float b) = Float(a ** b)

powNumber :: Number -> Number -> Interpreter -> Interpreter
powNumber (Number (Int val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Int(val1 ^ val2), numPos = pos1}) intptr
powNumber (Number (Int val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Float(fromIntegral val1 ** val2), numPos = pos1}) intptr
powNumber (Number (Float val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Float(val1 ** fromIntegral val2), numPos = pos1}) intptr
powNumber (Number (Float val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Float(val1 ** val2), numPos = pos1}) intptr
powNumber  (Number val1 pos1) (Number val2 pos2) intptr = intptr{intError = throwError (intError intptr) (InvalidOperation (intprFileName intptr) (powerOperation definedTypes) (printValueType val1) (printValueType val2) (fromJust pos1))}

sqrootNumber :: Number -> Interpreter -> Interpreter
sqrootNumber (Number (Int val1) pos1) intptr = setResult (Number {numberValue = Int(round (sqrt (fromIntegral val1))), numPos = pos1}) intptr
sqrootNumber (Number (Float val1) pos1) intptr = setResult (Number {numberValue = Float( sqrt val1), numPos = pos1}) intptr
sqrootNumber  (Number val1 pos1) intptr = intptr{intError = throwError (intError intptr) (InvalidSyntaxError (intprFileName intptr) ("\"integer or float\"") (printValueType val1) (fromJust pos1))}

indexNumber :: Number -> Number -> Interpreter -> Interpreter
indexNumber (Number (List val1) pos1) (Number (Int val2) pos2) intptr 
  | val2 < 0 && (length val1 + val2) >= 0 = setResult (Number {numberValue = (val1 !! (length val1 - val2)), numPos = pos1}) intptr 
  | val2 >= 0 && length val1 > val2 = setResult (Number {numberValue = (val1 !! val2), numPos = pos1}) intptr 
  | otherwise = intptr{intError = throwError (intError intptr) (OutOfBoundsIndex (intprFileName intptr) (show val2) (fromJust pos1))}
indexNumber  (Number val1 pos1) (Number val2 pos2) intptr = intptr{intError = throwError (intError intptr) (InvalidOperation (intprFileName intptr) (indexOperation definedTypes) (printValueType val1) (printValueType val2) (fromJust pos1))}

notNumber :: Number -> Interpreter -> Interpreter
notNumber (Number (Int val1) pos1) intptr = setResult (Number {numberValue = Int(complement val1), numPos = pos1}) intptr
notNumber (Number (Float val1) pos1) intptr = setResult (Number {numberValue = Int(complement (round val1)), numPos = pos1}) intptr
notNumber (Number (Bool val1) pos1) intptr = setResult (Number {numberValue = Bool(not val1), numPos = pos1}) intptr
notNumber (Number val1 pos1) intptr = intptr{intError = throwError (intError intptr)  (InvalidSyntaxError (intprFileName intptr) ("\"integer, float or boolean\"") (printValueType val1) (fromJust pos1))}

andNumber :: Number -> Number -> Interpreter -> Interpreter
andNumber (Number (Int val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Int(val1 .&. val2), numPos = pos1}) intptr
andNumber (Number (Float val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Int(round val1 .&. val2), numPos = pos1}) intptr
andNumber (Number (Int val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Int(val1 .&. round val2), numPos = pos1}) intptr
andNumber (Number (Float val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Int(round val1 .&. round val2), numPos = pos1}) intptr
andNumber (Number (Bool val1) pos1) (Number (Bool val2) pos2) intptr = setResult (Number {numberValue = Bool(val1 && val2), numPos = pos1}) intptr
andNumber (Number val1 pos1) (Number val2 pos2) intptr = intptr{intError = throwError (intError intptr) (InvalidOperation (intprFileName intptr) (andOperation definedTypes) (printValueType val1) (printValueType val2) (fromJust pos1))}

orNumber :: Number -> Number -> Interpreter -> Interpreter
orNumber (Number (Int val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Int(val1 .|. val2), numPos = pos1}) intptr
orNumber (Number (Float val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Int(round val1 .|. val2), numPos = pos1}) intptr
orNumber (Number (Int val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Int(val1 .|. round val2), numPos = pos1}) intptr
orNumber (Number (Float val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Int(round val1 .|. round val2), numPos = pos1}) intptr
orNumber (Number (Bool val1) pos1) (Number (Bool val2) pos2) intptr = setResult (Number {numberValue = Bool(val1 || val2), numPos = pos1}) intptr
orNumber (Number val1 pos1) (Number val2 pos2) intptr = intptr{intError = throwError (intError intptr) (InvalidOperation (intprFileName intptr) (orOperation definedTypes) (printValueType val1) (printValueType val2) (fromJust pos1))}

eqNumber :: Number -> Number -> Interpreter -> Interpreter
eqNumber (Number (Int val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue =  Bool( fromIntegral val1 == val2), numPos = pos1}) intptr
eqNumber (Number (Float val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Bool( val1 == fromIntegral val2), numPos = pos1}) intptr
eqNumber (Number (Bool val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Bool(fromEnum val1 == val2), numPos = pos1}) intptr
eqNumber (Number (Bool val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Bool(fromIntegral(fromEnum val1) == val2), numPos = pos1}) intptr
eqNumber (Number (Int val1) pos1) (Number (Bool val2) pos2) intptr = setResult (Number {numberValue = Bool(val1 == fromEnum val2), numPos = pos1}) intptr
eqNumber (Number (Float val1) pos1) (Number (Bool val2) pos2) intptr = setResult (Number {numberValue = Bool(val1 == fromIntegral(fromEnum val2)), numPos = pos1}) intptr
eqNumber (Number val1 pos1) (Number val2 pos2) intptr = setResult (Number {numberValue = Bool(val1 == val2), numPos = pos1}) intptr

neqNumber :: Number -> Number -> Interpreter -> Interpreter
neqNumber (Number (Int val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue =  Bool( fromIntegral val1 /= val2), numPos = pos1}) intptr
neqNumber (Number (Float val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Bool( val1 /= fromIntegral val2), numPos = pos1}) intptr
neqNumber (Number (Bool val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Bool(fromEnum val1 /= val2), numPos = pos1}) intptr
neqNumber (Number (Bool val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Bool(fromIntegral(fromEnum val1) /= val2), numPos = pos1}) intptr
neqNumber (Number (Int val1) pos1) (Number (Bool val2) pos2) intptr = setResult (Number {numberValue = Bool(val1 /= fromEnum val2), numPos = pos1}) intptr
neqNumber (Number (Float val1) pos1) (Number (Bool val2) pos2) intptr = setResult (Number {numberValue = Bool(val1 /= fromIntegral(fromEnum val2)), numPos = pos1}) intptr
neqNumber (Number val1 pos1) (Number val2 pos2) intptr = setResult (Number {numberValue = Bool(val1 /= val2), numPos = pos1}) intptr

greaterNumber :: Number -> Number -> Interpreter -> Interpreter
greaterNumber (Number (Int val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue =  Bool( fromIntegral val1 > val2), numPos = pos1}) intptr
greaterNumber (Number (Float val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Bool( val1 > fromIntegral val2), numPos = pos1}) intptr
greaterNumber (Number (Bool val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Bool(fromEnum val1 > val2), numPos = pos1}) intptr
greaterNumber (Number (Bool val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Bool(fromIntegral(fromEnum val1) > val2), numPos = pos1}) intptr
greaterNumber (Number (Int val1) pos1) (Number (Bool val2) pos2) intptr = setResult (Number {numberValue = Bool(val1 > fromEnum val2), numPos = pos1}) intptr
greaterNumber (Number (Float val1) pos1) (Number (Bool val2) pos2) intptr = setResult (Number {numberValue = Bool(val1 > fromIntegral(fromEnum val2)), numPos = pos1}) intptr
greaterNumber (Number val1 pos1) (Number val2 pos2) intptr = setResult (Number {numberValue = Bool(val1 > val2), numPos = pos1}) intptr

lessNumber :: Number -> Number -> Interpreter -> Interpreter
lessNumber (Number (Int val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue =  Bool( fromIntegral val1 < val2), numPos = pos1}) intptr
lessNumber (Number (Float val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Bool( val1 < fromIntegral val2), numPos = pos1}) intptr
lessNumber (Number (Bool val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Bool(fromEnum val1 < val2), numPos = pos1}) intptr
lessNumber (Number (Bool val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Bool(fromIntegral(fromEnum val1) < val2), numPos = pos1}) intptr
lessNumber (Number (Int val1) pos1) (Number (Bool val2) pos2) intptr = setResult (Number {numberValue = Bool(val1 < fromEnum val2), numPos = pos1}) intptr
lessNumber (Number (Float val1) pos1) (Number (Bool val2) pos2) intptr = setResult (Number {numberValue = Bool(val1 < fromIntegral(fromEnum val2)), numPos = pos1}) intptr
lessNumber (Number val1 pos1) (Number val2 pos2) intptr = setResult (Number {numberValue = Bool(val1 < val2), numPos = pos1}) intptr

greaterEqNumber :: Number -> Number -> Interpreter -> Interpreter
greaterEqNumber (Number (Int val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue =  Bool( fromIntegral val1 >= val2), numPos = pos1}) intptr
greaterEqNumber (Number (Float val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Bool( val1 >= fromIntegral val2), numPos = pos1}) intptr
greaterEqNumber (Number (Bool val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Bool(fromEnum val1 >= val2), numPos = pos1}) intptr
greaterEqNumber (Number (Bool val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Bool(fromIntegral(fromEnum val1) >= val2), numPos = pos1}) intptr
greaterEqNumber (Number (Int val1) pos1) (Number (Bool val2) pos2) intptr = setResult (Number {numberValue = Bool(val1 >= fromEnum val2), numPos = pos1}) intptr
greaterEqNumber (Number (Float val1) pos1) (Number (Bool val2) pos2) intptr = setResult (Number {numberValue = Bool(val1 >= fromIntegral(fromEnum val2)), numPos = pos1}) intptr
greaterEqNumber (Number val1 pos1) (Number val2 pos2) intptr = setResult (Number {numberValue = Bool(val1 >= val2), numPos = pos1}) intptr

lessEqNumber :: Number -> Number -> Interpreter -> Interpreter
lessEqNumber (Number (Int val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue =  Bool( fromIntegral val1 <= val2), numPos = pos1}) intptr
lessEqNumber (Number (Float val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Bool( val1 <= fromIntegral val2), numPos = pos1}) intptr
lessEqNumber (Number (Bool val1) pos1) (Number (Int val2) pos2) intptr = setResult (Number {numberValue = Bool(fromEnum val1 <= val2), numPos = pos1}) intptr
lessEqNumber (Number (Bool val1) pos1) (Number (Float val2) pos2) intptr = setResult (Number {numberValue = Bool(fromIntegral(fromEnum val1) <= val2), numPos = pos1}) intptr
lessEqNumber (Number (Int val1) pos1) (Number (Bool val2) pos2) intptr = setResult (Number {numberValue = Bool(val1 <= fromEnum val2), numPos = pos1}) intptr
lessEqNumber (Number (Float val1) pos1) (Number (Bool val2) pos2) intptr = setResult (Number {numberValue = Bool(val1 <= fromIntegral(fromEnum val2)), numPos = pos1}) intptr
lessEqNumber (Number val1 pos1) (Number val2 pos2) intptr = setResult (Number {numberValue = Bool(val1 <= val2), numPos = pos1}) intptr

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

mapVisit :: (Interpreter -> b) -> Node -> Interpreter -> [b]
mapVisit f (Empty) intptr = []
mapVisit f (Leaf tok typ) intptr = []
mapVisit f (Branch tok typ r1) intptr = [(f (visit r1 intptr))] 
mapVisit f (Tree l1 tok typ r1) intptr = [(f (visit l1 intptr))] ++ (mapVisit f r1 intptr)  

setResult :: Number -> Interpreter -> Interpreter
setResult num inter =
  inter{currentResult = Just num}

visit :: Node -> Interpreter -> Interpreter
visit node intptr 
  | hasOccurred (intError intptr) = intptr 
  | typ == SeperatorNode = visitStatement node intptr 
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
  | typ == ListNode = visitListNode node intptr
  | typ == PrintNode = visitPrintNode node intptr 
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
  | tokenType tok == sqrootOperation definedTypes =  sqrootNumber num2 intptr
  | hasOccurred (intError visitLeft) = intptr{intError = (intError visitLeft)}
  | tokenType tok == plusOperation definedTypes = addNumber num1 num2 intptr
  | tokenType tok == minusOperation definedTypes = subNumber num1 num2 intptr
  | tokenType tok == multiplyOperation definedTypes = mulNumber num1 num2 intptr
  | tokenType tok == divisionOperation definedTypes = numCheck
  | tokenType tok == modOperation definedTypes = modNumber num1 num2 intptr
  | tokenType tok == powerOperation definedTypes = powNumber num1 num2 intptr
  | tokenType tok == indexOperation definedTypes = indexNumber num1 num2 intptr
  | tokenType tok == andOperation definedTypes = andNumber num1 num2 intptr
  | tokenType tok == orOperation definedTypes = orNumber num1 num2 intptr
  | tokenType tok == equalOperation definedTypes = eqNumber num1 num2 intptr
  | tokenType tok == notEqualOperation definedTypes = neqNumber num1 num2 intptr
  | tokenType tok == greaterOperation definedTypes = greaterNumber num1 num2 intptr
  | tokenType tok == lessOperation definedTypes = lessNumber num1 num2 intptr
  | tokenType tok == greaterEqOperation definedTypes = greaterEqNumber num1 num2 intptr
  | tokenType tok == lessEqOperation definedTypes = lessEqNumber num1 num2 intptr
  where 
    visitLeft = visit left intptr
    visitRight = visit right intptr
    num1 = fromJust(currentResult (visitLeft))
    num2 = fromJust(currentResult (visitRight))
    numCheck = 
      if isZero (numberValue num2)
      then intptr{intError = throwError (intError intptr) (DivisionByZeroError (intprFileName intptr) (pos tok))}
      else divNumber num1 num2 intptr

visitUnaryNode :: Node -> Interpreter -> Interpreter
visitUnaryNode node@(Branch _ _ right) intptr
  | hasOccurred (intError visitValue) = intptr{intError = intError visitValue} 
  | tokenType tok == minusOperation definedTypes = mulNumber num1 Number{numberValue = Int(-1), numPos = Nothing} intptr
  | tokenType tok == notOperation definedTypes = notNumber num1 intptr
  | otherwise = setResult num1 intptr
  where 
    visitValue = visit right intptr
    num1 = fromJust(currentResult visitValue)
    tok = getToken node

visitStatement :: Node -> Interpreter -> Interpreter
visitStatement Empty intptr = intptr 
visitStatement (Tree left tok typ right) intptr = visit right (visit left intptr) 
visitStatement node intptr = error (show node)

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
 | conditionResult = setResult ifResult ifStatement
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
    newIndex = fromJust (currentResult(addNumber fromValue withValue intptr))
    nextIteration = visit statementNode intptr


visitUntilNode :: Node -> Interpreter -> Interpreter
visitUntilNode node@(Tree left _ _ right) intptr 
  | hasOccurred (intError conditionVisit) = intptr{intError = intError conditionVisit}
  | snd (getBoolValue (numberValue conditionResult) intptr left) = intptr
  | otherwise = visitUntilNode node nextIteration
  where 
    conditionVisit = visit left intptr
    conditionResult = fromJust(currentResult (conditionVisit))
    nextIteration = visitStatement right intptr

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

visitListNode :: Node -> Interpreter -> Interpreter
visitListNode (Branch tok typ r1) intptr = intptr{ currentResult = Just Number{numberValue = List getValues, numPos = Nothing}}
  where
    getValues = mapVisit (numberValue . fromJust . currentResult) r1 intptr 

visitPrintNode :: Node -> Interpreter -> Interpreter
visitPrintNode (Branch _ _ right) intptr  
  | currentResult newIntpr == Nothing = newIntpr
  | otherwise = newIntpr {printResultList = (printResultList newIntpr) ++ [(currentResult newIntpr)], currentResult = Nothing}
  where
    newIntpr = visit right intptr 
    
 
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
    newIntptr = visitStatement stat funcIntptr

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
  runInterpreter result{currentResult = Nothing, printResultList = []}

checkResult :: Interpreter -> IO ()
checkResult inter = do 
  if  hasOccurred (intError inter)
  then putStrLn (concat (intersperse "\n" (errorMessage (intError inter)))) 
  else mapM_ printResult ((printResultList inter) ++ [(currentResult inter)])

printResult :: Maybe Number -> IO ()
printResult a 
  | isJust a = putStrLn( show (numberValue (fromJust a)))
  | otherwise = putStr("")
      

