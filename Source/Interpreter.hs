{- 
Module      : Interpreter 
Description : The interpreter of the Mallaka Language which converts the parse tree into a result. 
Maintainer  : Tobias van den Hoogen  
-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Source.Interpreter(module Source.Interpreter) where
import System.IO
import Data.Maybe
import Data.Fixed
import Control.Monad
import Data.Map hiding (null, map, foldr, drop, take, fold)
import Data.Bits
import qualified Data.List hiding (lookup, insert, delete)
import Prelude hiding (lookup)
import Source.Parser
import Source.Lexer
import Source.Exception
import System.Directory
import System.FilePath.Posix
import Source.Types
import GHC.Float (int2Float)


-- | The interpreter of the Mallaka Language 
data Interpreter = Interpreter
  {intprFileName :: String
  , intEnv :: Environment
  , intError :: Error
  , currentResult :: Maybe Result
  , printResultList :: [Maybe Result]
  }
  deriving stock Show

-- | The environment of the Mallaka Language which stores the variables
data Environment = Environment{
  lookupTable :: Map String Value,
  parent :: Maybe Environment
}
  deriving stock Show



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



getBoolValue :: Value -> Interpreter -> Node -> (Interpreter ,Bool)
getBoolValue (BoolValue a) i _ = (i, a)
getBoolValue _ i n =  (i{intError = throwError (intError i) (ConditionError(intprFileName i) (tokenPos (getToken n)))}, True)

addResult :: Result -> Result -> Interpreter -> Interpreter
addResult (Result (IntValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = IntValue(val1 + val2), resultPos = pos1}) intptr
addResult (Result (IntValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = FloatValue(fromIntegral val1 + val2), resultPos = pos1}) intptr
addResult (Result (FloatValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = FloatValue(val1 + fromIntegral val2), resultPos = pos1}) intptr
addResult (Result (FloatValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = FloatValue(val1 + val2), resultPos = pos1}) intptr
addResult (Result (StringValue val1) pos1) (Result val2 _) intptr = setResult (Result {resultValue = StringValue(val1 ++ show val2), resultPos = pos1}) intptr
addResult (Result (ListValue val1) pos1) (Result val2 _) intptr = setResult (Result {resultValue = ListValue(val1 ++ [val2]), resultPos = pos1}) intptr
addResult (Result val1 pos1) (Result val2 _) intptr = intptr{intError = throwError (intError intptr) (InvalidOperation (intprFileName intptr) (plusOperation definedTypes) (getValueType val1) (getValueType val2) (fromJust pos1))}

subResult :: Result -> Result -> Interpreter -> Interpreter
subResult (Result (IntValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = IntValue(val1 - val2), resultPos = pos1}) intptr
subResult (Result (IntValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = FloatValue(fromIntegral val1 - val2), resultPos = pos1}) intptr
subResult (Result (FloatValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = FloatValue(val1 - fromIntegral val2), resultPos = pos1}) intptr
subResult (Result (FloatValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = FloatValue(val1 - val2), resultPos = pos1}) intptr
subResult (Result (ListValue val1) pos1) (Result (IntValue val2) _) intptr
  | val2 < 0 && (length val1 + val2) >= 0 = setResult (Result {resultValue = ListValue(take (length val1 + val2) val1 ++ drop (length val1 + val2 + 1) val1), resultPos = pos1}) intptr
  | val2 >= 0 && length val1 > val2 = setResult (Result {resultValue = ListValue(take val2 val1 ++ drop (val2 + 1) val1) , resultPos = pos1}) intptr
  | otherwise = intptr{intError = throwError (intError intptr) (OutOfBoundsIndex (intprFileName intptr) (show val2) (fromJust pos1))}
subResult  (Result val1 pos1) (Result val2 _) intptr = intptr{intError = throwError (intError intptr) (InvalidOperation (intprFileName intptr) (minusOperation definedTypes) (getValueType val1) (getValueType val2) (fromJust pos1))}

mulResult :: Result -> Result -> Interpreter -> Interpreter
mulResult (Result (IntValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = IntValue(val1 * val2), resultPos = pos1}) intptr
mulResult (Result (IntValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = FloatValue(fromIntegral val1 * val2), resultPos = pos1}) intptr
mulResult (Result (FloatValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = FloatValue(val1 * fromIntegral val2), resultPos = pos1}) intptr
mulResult (Result (FloatValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = FloatValue(val1 * val2), resultPos = pos1}) intptr
mulResult (Result (StringValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = StringValue(concat (replicate val2 val1)), resultPos = pos1}) intptr
mulResult (Result (ListValue val1) pos1) (Result (ListValue val2) _) intptr = setResult (Result {resultValue = ListValue(val1 ++ val2), resultPos = pos1}) intptr
mulResult (Result val1 pos1) (Result val2 _) intptr = intptr{intError = throwError (intError intptr) (InvalidOperation (intprFileName intptr) (multiplyOperation definedTypes) (getValueType val1) (getValueType val2) (fromJust pos1))}

divResult :: Result -> Result -> Interpreter -> Interpreter
divResult (Result (IntValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = IntValue(val1 `div` val2), resultPos = pos1}) intptr
divResult (Result (IntValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = FloatValue(fromIntegral val1 / val2), resultPos = pos1}) intptr
divResult (Result (FloatValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = FloatValue(val1 / fromIntegral val2), resultPos = pos1}) intptr
divResult (Result (FloatValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = FloatValue(val1 / val2), resultPos = pos1}) intptr
divResult  (Result val1 pos1) (Result val2 _) intptr = intptr{intError = throwError (intError intptr) (InvalidOperation (intprFileName intptr) (divisionOperation definedTypes) (getValueType val1) (getValueType val2) (fromJust pos1))}

modResult :: Result -> Result -> Interpreter -> Interpreter
modResult (Result (IntValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = IntValue(val1 `mod` val2), resultPos = pos1}) intptr
modResult (Result (IntValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = FloatValue( mod' (fromIntegral val1) val2), resultPos = pos1}) intptr
modResult (Result (FloatValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = FloatValue( mod' val1 (fromIntegral val2)), resultPos = pos1}) intptr
modResult (Result (FloatValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = FloatValue( mod' val1 val2), resultPos = pos1}) intptr
modResult  (Result val1 pos1) (Result val2 _) intptr = intptr{intError = throwError (intError intptr) (InvalidOperation (intprFileName intptr) (modOperation definedTypes) (getValueType val1) (getValueType val2) (fromJust pos1))}

powValue :: Value -> Value -> Value
powValue (IntValue a) (IntValue b) = IntValue( a ^ b)
powValue (IntValue a) (FloatValue b) = FloatValue( fromIntegral a ** b)
powValue (FloatValue a) (IntValue b) = FloatValue( a ** fromIntegral b)
powValue (FloatValue a) (FloatValue b) = FloatValue(a ** b)
powValue a _ = a

powResult :: Result -> Result -> Interpreter -> Interpreter
powResult (Result (IntValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = IntValue(val1 ^ val2), resultPos = pos1}) intptr
powResult (Result (IntValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = FloatValue(fromIntegral val1 ** val2), resultPos = pos1}) intptr
powResult (Result (FloatValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = FloatValue(val1 ** fromIntegral val2), resultPos = pos1}) intptr
powResult (Result (FloatValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = FloatValue(val1 ** val2), resultPos = pos1}) intptr
powResult  (Result val1 pos1) (Result val2 _) intptr = intptr{intError = throwError (intError intptr) (InvalidOperation (intprFileName intptr) (powerOperation definedTypes) (getValueType val1) (getValueType val2) (fromJust pos1))}

sqrootResult :: Result -> Interpreter -> Interpreter
sqrootResult (Result (IntValue val1) pos1) intptr = setResult (Result {resultValue = IntValue(round (sqrt (int2Float val1))), resultPos = pos1}) intptr
sqrootResult (Result (FloatValue val1) pos1) intptr = setResult (Result {resultValue = FloatValue( sqrt val1), resultPos = pos1}) intptr
sqrootResult (Result (ListValue val1) pos1) intptr = setResult (Result {resultValue = IntValue (length val1), resultPos = pos1}) intptr
sqrootResult  (Result val1 pos1) intptr = intptr{intError = throwError (intError intptr) (InvalidSyntaxError (intprFileName intptr) "\"integer or float\"" (getValueType val1) (fromJust pos1))}

indexResult :: Result -> Result -> Interpreter -> Interpreter
indexResult (Result (ListValue val1) pos1) (Result (IntValue val2) _) intptr
  | val2 < 0 && (length val1 + val2) >= 0 = setResult (Result {resultValue = val1 !! (length val1 - val2), resultPos = pos1}) intptr
  | val2 >= 0 && length val1 > val2 = setResult (Result {resultValue = val1 !! val2, resultPos = pos1}) intptr
  | otherwise = intptr{intError = throwError (intError intptr) (OutOfBoundsIndex (intprFileName intptr) (show val2) (fromJust pos1))}
indexResult  (Result val1 pos1) (Result val2 _) intptr = intptr{intError = throwError (intError intptr) (InvalidOperation (intprFileName intptr) (indexOperation definedTypes) (getValueType val1) (getValueType val2) (fromJust pos1))}

notResult :: Result -> Interpreter -> Interpreter
notResult (Result (IntValue val1) pos1) intptr = setResult (Result {resultValue = IntValue(complement val1), resultPos = pos1}) intptr
notResult (Result (FloatValue val1) pos1) intptr = setResult (Result {resultValue = IntValue(complement (round val1)), resultPos = pos1}) intptr
notResult (Result (BoolValue val1) pos1) intptr = setResult (Result {resultValue = BoolValue(not val1), resultPos = pos1}) intptr
notResult (Result val1 pos1) intptr = intptr{intError = throwError (intError intptr)  (InvalidSyntaxError (intprFileName intptr) "\"integer, floatValue or boolean\"" (getValueType val1) (fromJust pos1))}

andResult :: Result -> Result -> Interpreter -> Interpreter
andResult (Result (IntValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = IntValue(val1 .&. val2), resultPos = pos1}) intptr
andResult (Result (FloatValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = IntValue(round val1 .&. val2), resultPos = pos1}) intptr
andResult (Result (IntValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = IntValue(val1 .&. round val2), resultPos = pos1}) intptr
andResult (Result (FloatValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = IntValue(round val1 .&. round val2), resultPos = pos1}) intptr
andResult (Result (BoolValue val1) pos1) (Result (BoolValue val2) _) intptr = setResult (Result {resultValue = BoolValue(val1 && val2), resultPos = pos1}) intptr
andResult (Result val1 pos1) (Result val2 _) intptr = intptr{intError = throwError (intError intptr) (InvalidOperation (intprFileName intptr) (andOperation definedTypes) (getValueType val1) (getValueType val2) (fromJust pos1))}

orResult :: Result -> Result -> Interpreter -> Interpreter
orResult (Result (IntValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = IntValue(val1 .|. val2), resultPos = pos1}) intptr
orResult (Result (FloatValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = IntValue(round val1 .|. val2), resultPos = pos1}) intptr
orResult (Result (IntValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = IntValue(val1 .|. round val2), resultPos = pos1}) intptr
orResult (Result (FloatValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = IntValue(round val1 .|. round val2), resultPos = pos1}) intptr
orResult (Result (BoolValue val1) pos1) (Result (BoolValue val2) _) intptr = setResult (Result {resultValue = BoolValue(val1 || val2), resultPos = pos1}) intptr
orResult (Result val1 pos1) (Result val2 _) intptr = intptr{intError = throwError (intError intptr) (InvalidOperation (intprFileName intptr) (orOperation definedTypes) (getValueType val1) (getValueType val2) (fromJust pos1))}

eqResult :: Result -> Result -> Interpreter -> Interpreter
eqResult (Result (IntValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue =  BoolValue( fromIntegral val1 == val2), resultPos = pos1}) intptr
eqResult (Result (FloatValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = BoolValue( val1 == fromIntegral val2), resultPos = pos1}) intptr
eqResult (Result (BoolValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = BoolValue(fromEnum val1 == val2), resultPos = pos1}) intptr
eqResult (Result (BoolValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = BoolValue(fromIntegral(fromEnum val1) == val2), resultPos = pos1}) intptr
eqResult (Result (IntValue val1) pos1) (Result (BoolValue val2) _) intptr = setResult (Result {resultValue = BoolValue(val1 == fromEnum val2), resultPos = pos1}) intptr
eqResult (Result (FloatValue val1) pos1) (Result (BoolValue val2) _) intptr = setResult (Result {resultValue = BoolValue(val1 == fromIntegral(fromEnum val2)), resultPos = pos1}) intptr
eqResult (Result val1 pos1) (Result val2 _) intptr = setResult (Result {resultValue = BoolValue(val1 == val2), resultPos = pos1}) intptr

neqResult :: Result -> Result -> Interpreter -> Interpreter
neqResult (Result (IntValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue =  BoolValue( fromIntegral val1 /= val2), resultPos = pos1}) intptr
neqResult (Result (FloatValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = BoolValue( val1 /= fromIntegral val2), resultPos = pos1}) intptr
neqResult (Result (BoolValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = BoolValue(fromEnum val1 /= val2), resultPos = pos1}) intptr
neqResult (Result (BoolValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = BoolValue(fromIntegral(fromEnum val1) /= val2), resultPos = pos1}) intptr
neqResult (Result (IntValue val1) pos1) (Result (BoolValue val2) _) intptr = setResult (Result {resultValue = BoolValue(val1 /= fromEnum val2), resultPos = pos1}) intptr
neqResult (Result (FloatValue val1) pos1) (Result (BoolValue val2) _) intptr = setResult (Result {resultValue = BoolValue(val1 /= fromIntegral(fromEnum val2)), resultPos = pos1}) intptr
neqResult (Result val1 pos1) (Result val2 _) intptr = setResult (Result {resultValue = BoolValue(val1 /= val2), resultPos = pos1}) intptr

greaterResult :: Result -> Result -> Interpreter -> Interpreter
greaterResult (Result (IntValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue =  BoolValue( fromIntegral val1 > val2), resultPos = pos1}) intptr
greaterResult (Result (FloatValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = BoolValue( val1 > fromIntegral val2), resultPos = pos1}) intptr
greaterResult (Result (BoolValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = BoolValue(fromEnum val1 > val2), resultPos = pos1}) intptr
greaterResult (Result (BoolValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = BoolValue(fromIntegral(fromEnum val1) > val2), resultPos = pos1}) intptr
greaterResult (Result (IntValue val1) pos1) (Result (BoolValue val2) _) intptr = setResult (Result {resultValue = BoolValue(val1 > fromEnum val2), resultPos = pos1}) intptr
greaterResult (Result (FloatValue val1) pos1) (Result (BoolValue val2) _) intptr = setResult (Result {resultValue = BoolValue(val1 > fromIntegral(fromEnum val2)), resultPos = pos1}) intptr
greaterResult (Result val1 pos1) (Result val2 _) intptr = setResult (Result {resultValue = BoolValue(val1 > val2), resultPos = pos1}) intptr

lessResult :: Result -> Result -> Interpreter -> Interpreter
lessResult (Result (IntValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue =  BoolValue( fromIntegral val1 < val2), resultPos = pos1}) intptr
lessResult (Result (FloatValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = BoolValue( val1 < fromIntegral val2), resultPos = pos1}) intptr
lessResult (Result (BoolValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = BoolValue(fromEnum val1 < val2), resultPos = pos1}) intptr
lessResult (Result (BoolValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = BoolValue(fromIntegral(fromEnum val1) < val2), resultPos = pos1}) intptr
lessResult (Result (IntValue val1) pos1) (Result (BoolValue val2) _) intptr = setResult (Result {resultValue = BoolValue(val1 < fromEnum val2), resultPos = pos1}) intptr
lessResult (Result (FloatValue val1) pos1) (Result (BoolValue val2) _) intptr = setResult (Result {resultValue = BoolValue(val1 < fromIntegral(fromEnum val2)), resultPos = pos1}) intptr
lessResult (Result val1 pos1) (Result val2 _) intptr = setResult (Result {resultValue = BoolValue(val1 < val2), resultPos = pos1}) intptr

greaterEqResult :: Result -> Result -> Interpreter -> Interpreter
greaterEqResult (Result (IntValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue =  BoolValue( fromIntegral val1 >= val2), resultPos = pos1}) intptr
greaterEqResult (Result (FloatValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = BoolValue( val1 >= fromIntegral val2), resultPos = pos1}) intptr
greaterEqResult (Result (BoolValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = BoolValue(fromEnum val1 >= val2), resultPos = pos1}) intptr
greaterEqResult (Result (BoolValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = BoolValue(fromIntegral(fromEnum val1) >= val2), resultPos = pos1}) intptr
greaterEqResult (Result (IntValue val1) pos1) (Result (BoolValue val2) _) intptr = setResult (Result {resultValue = BoolValue(val1 >= fromEnum val2), resultPos = pos1}) intptr
greaterEqResult (Result (FloatValue val1) pos1) (Result (BoolValue val2) _) intptr = setResult (Result {resultValue = BoolValue(val1 >= fromIntegral(fromEnum val2)), resultPos = pos1}) intptr
greaterEqResult (Result val1 pos1) (Result val2 _) intptr = setResult (Result {resultValue = BoolValue(val1 >= val2), resultPos = pos1}) intptr

lessEqResult :: Result -> Result -> Interpreter -> Interpreter
lessEqResult (Result (IntValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue =  BoolValue( fromIntegral val1 <= val2), resultPos = pos1}) intptr
lessEqResult (Result (FloatValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = BoolValue( val1 <= fromIntegral val2), resultPos = pos1}) intptr
lessEqResult (Result (BoolValue val1) pos1) (Result (IntValue val2) _) intptr = setResult (Result {resultValue = BoolValue(fromEnum val1 <= val2), resultPos = pos1}) intptr
lessEqResult (Result (BoolValue val1) pos1) (Result (FloatValue val2) _) intptr = setResult (Result {resultValue = BoolValue(fromIntegral(fromEnum val1) <= val2), resultPos = pos1}) intptr
lessEqResult (Result (IntValue val1) pos1) (Result (BoolValue val2) _) intptr = setResult (Result {resultValue = BoolValue(val1 <= fromEnum val2), resultPos = pos1}) intptr
lessEqResult (Result (FloatValue val1) pos1) (Result (BoolValue val2) _) intptr = setResult (Result {resultValue = BoolValue(val1 <= fromIntegral(fromEnum val2)), resultPos = pos1}) intptr
lessEqResult (Result val1 pos1) (Result val2 _) intptr = setResult (Result {resultValue = BoolValue(val1 <= val2), resultPos = pos1}) intptr

isZero :: Value-> Bool
isZero (IntValue a)
  | a == 0 = True
  | otherwise = False
isZero (FloatValue a)
  | a == 0.0 = True
  | otherwise = False
isZero _ = False

zipMapNodeTree :: (Node -> Interpreter -> Interpreter) -> [c] -> Node -> Interpreter -> [(c, Result)]
zipMapNodeTree _ [] _ _ = []
zipMapNodeTree f a e@Empty c = [(head a,  getResultSafe(currentResult (f e c)))]
zipMapNodeTree f a l@(Leaf _ _)  c = [(head a, getResultSafe(currentResult (f l c)))]
zipMapNodeTree f a b@(Branch _ _ r1) c = (head a, getResultSafe(currentResult (f b c))) : zipMapNodeTree f (tail a) r1 c
zipMapNodeTree f a (Tree l1 _ _ r1) c = (head a, getResultSafe(currentResult (f l1 c))) : zipMapNodeTree f (tail a) r1 c

mapVisit :: (Interpreter -> b) -> Node -> Interpreter -> [b]
mapVisit _ Empty _ = []
mapVisit _ (Leaf _ _) _ = []
mapVisit f (Branch _ _ r1) intptr = [f (visit r1 intptr)]
mapVisit f (Tree l1 _ _ r1) intptr = f (visit l1 intptr) : mapVisit f r1 intptr

setResult :: Result -> Interpreter -> Interpreter
setResult num inter =
  inter{currentResult = Just num}

visit :: Node -> Interpreter -> Interpreter
visit node intptr
  | hasOccurred (intError intptr) = intptr
  | typ == SeparatorNode = visitStatement node intptr
  | typ == ValueNode = visitResultNode node intptr
  | typ == VarAccessNode = visitVarAccessNode node intptr
  | typ == VarAssignNode = visitVarAssignNode node intptr
  | typ == BinaryOpNode = visitBinaryOpNode node intptr
  | typ == UnaryNode = visitUnaryNode node intptr
  | typ == IndexAssignNode = visitIndexAssignNode node intptr
  | typ == IfNode = visitIfNode node intptr
  | typ == LoopNode = visitLoopNode node intptr
  | typ == UntilNode = visitUntilNode node intptr
  | typ == FunctionAssignNode = visitCreateFunctionNode node intptr
  | typ == FunctionRunNode = visitRunFunctionNode node intptr
  | typ == ListNode = visitListNode node intptr
  | typ == PrintNode = visitPrintNode node intptr
  | otherwise = error "bruh"--(show node)
  where
    typ = getNodeType node

visitResultNode :: Node -> Interpreter -> Interpreter
visitResultNode node intptr = case tokenVal tok of
  (Just _) -> setResult (Result{resultValue = fromJust(tokenVal tok), resultPos = Just (tokenPos tok)}) intptr
  Nothing -> intptr
  where
    tok = getToken node

visitVarAccessNode :: Node -> Interpreter -> Interpreter
visitVarAccessNode node intptr =
  if isJust value
  then setResult Result{resultValue = fromJust value, resultPos = Just (tokenPos tok)} intptr
  else intptr{intError = throwError (intError intptr) (NotDefinedError (intprFileName intptr) ( "\"" ++ show(fromJust(tokenVal tok)) ++ "\"") (tokenPos tok))}
  where
    tok = getToken node
    value = getEnvironmentValue (intEnv intptr) (show(fromJust(tokenVal tok)))

visitVarAssignNode :: Node -> Interpreter -> Interpreter
visitVarAssignNode (Tree left _ _ right) intptr
  | hasOccurred (intError visitValue) = intptr{intError = intError visitValue}
  | otherwise = newEnv
  where
    visitValue = visit right intptr
    valueNode = getResultSafe(currentResult visitValue)
    varName = show(fromJust(tokenVal (getToken left)))
    value = setResult valueNode intptr
    valueResult = fromJust(currentResult value)
    newEnv = value {intEnv = setEnvironmentValue (intEnv value) varName (resultValue valueResult)}
visitVarAssignNode _ intptr = intptr{intError = throwError (intError intptr) (DeveloperException (intprFileName intptr))}

visitIndexAssignNode :: Node -> Interpreter -> Interpreter
visitIndexAssignNode (Tree (Tree l1 _ _ l2) _ _ right) intptr
  | hasOccurred (intError value) = intptr{intError = intError indexValue}
  | tokenType (getToken l1) == identifier definedTypes = visitAssignIndexValue (fromJust(tokenVal (getToken l1))) (getValueSafe (currentResult indexValue)) (getValueSafe (currentResult value)) intptr (fromJust (resultPos (fromJust (currentResult lst))))
  | otherwise = visitAssignIndexValue (getValueSafe (currentResult lst)) (getValueSafe (currentResult indexValue)) (getValueSafe (currentResult value)) intptr (fromJust (resultPos (fromJust (currentResult lst))))
  where
    lst = visit l1 intptr
    indexValue = visit l2 lst
    value = visit right intptr
visitIndexAssignNode _ intptr = intptr{intError = throwError (intError intptr) (DeveloperException (intprFileName intptr))}

visitAssignIndexValue :: Value -> Value -> Value -> Interpreter -> Position -> Interpreter
visitAssignIndexValue (ListValue val1) (IntValue val2) _ intptr _ = setResult Result{ resultValue = val1 !! val2, resultPos = Nothing} intptr
visitAssignIndexValue (StringValue val1) (IntValue val2) val3 intptr position =
  case getList of
    Just (ListValue value) ->
      if val2 < 0 && (length value + val2) >= 0 then
        intptr{ intEnv = setEnvironmentValue (intEnv intptr)  val1 (ListValue (take val2 value ++ [val3] ++ drop (val2 + 1) value)),
                 currentResult = Just (Result (ListValue (take (length value + val2) value ++ [val3] ++ drop (length value + val2 + 1) value)) Nothing)}
        else
          if val2 >= 0 && length value > val2 then
            intptr{ intEnv = setEnvironmentValue (intEnv intptr)  val1 (ListValue (take val2 value ++ [val3] ++ drop (val2 + 1) value)),
                 currentResult = Just (Result (ListValue (take val2 value ++ [val3] ++ drop (val2 + 1) value)) Nothing)}
          else intptr{intError = throwError (intError intptr) (OutOfBoundsIndex (intprFileName intptr) (show val2) position)}
    Just value -> intptr{intError = throwError (intError intptr) (InvalidSyntaxError (intprFileName intptr) "List" ( "\"" ++ getValueType value ++ "\"")  position)}
    _ -> intptr{intError = throwError (intError intptr) (NotDefinedError (intprFileName intptr) ( "\"" ++ val1 ++ "\"") position)}

  where
    getList = getEnvironmentValue (intEnv intptr) val1
visitAssignIndexValue _ _ _ intptr _ = intptr{intError = throwError (intError intptr) (DeveloperException (intprFileName intptr))}

visitBinaryOpNode :: Node -> Interpreter -> Interpreter
visitBinaryOpNode (Tree left tok _ right) intptr
  | hasOccurred (intError visitRight) = intptr{intError = intError visitRight}
  | tokenType tok == sqrootOperation definedTypes =  sqrootResult num2 intptr
  | hasOccurred (intError visitLeft) = intptr{intError = intError visitLeft}
  | tokenType tok == plusOperation definedTypes = addResult num1 num2 intptr
  | tokenType tok == minusOperation definedTypes = subResult num1 num2 intptr
  | tokenType tok == multiplyOperation definedTypes = mulResult num1 num2 intptr
  | tokenType tok == divisionOperation definedTypes = numCheck
  | tokenType tok == modOperation definedTypes = modResult num1 num2 intptr
  | tokenType tok == powerOperation definedTypes = powResult num1 num2 intptr
  | tokenType tok == indexOperation definedTypes = indexResult num1 num2 intptr
  | tokenType tok == andOperation definedTypes = andResult num1 num2 intptr
  | tokenType tok == orOperation definedTypes = orResult num1 num2 intptr
  | tokenType tok == equalOperation definedTypes = eqResult num1 num2 intptr
  | tokenType tok == notEqualOperation definedTypes = neqResult num1 num2 intptr
  | tokenType tok == greaterOperation definedTypes = greaterResult num1 num2 intptr
  | tokenType tok == lessOperation definedTypes = lessResult num1 num2 intptr
  | tokenType tok == greaterEqOperation definedTypes = greaterEqResult num1 num2 intptr
  | tokenType tok == lessEqOperation definedTypes = lessEqResult num1 num2 intptr
  where
    visitLeft = visit left intptr
    visitRight = visit right intptr
    num1 = fromJust(currentResult visitLeft)
    num2 = fromJust(currentResult visitRight)
    numCheck =
      if isZero (resultValue num2)
      then intptr{intError = throwError (intError intptr) (DivisionByZeroError (intprFileName intptr) (tokenPos tok))}
      else divResult num1 num2 intptr
visitBinaryOpNode _ intptr = intptr{intError = throwError (intError intptr) (DeveloperException (intprFileName intptr))}

visitUnaryNode :: Node -> Interpreter -> Interpreter
visitUnaryNode node@(Branch _ _ right) intptr
  | hasOccurred (intError visitValue) = intptr{intError = intError visitValue}
  | tokenType tok == minusOperation definedTypes = mulResult num1 Result{resultValue = IntValue(-1), resultPos = Nothing} intptr
  | tokenType tok == notOperation definedTypes = notResult num1 intptr
  | otherwise = setResult num1 intptr
  where
    visitValue = visit right intptr
    num1 = fromJust(currentResult visitValue)
    tok = getToken node
visitUnaryNode _ intptr =intptr{intError = throwError (intError intptr) (DeveloperException (intprFileName intptr))}

visitStatement :: Node -> Interpreter -> Interpreter
visitStatement Empty intptr = intptr
visitStatement (Tree left _ _ right) intptr = visit right (visit left intptr)
visitStatement node _ = error (show node)

visitIfNode :: Node -> Interpreter -> Interpreter
visitIfNode node@(Tree left tok _ _) intptr
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
visitIfConditionNode (Tree (Tree leftLeft _ _ leftRight) _ _ right) intptr
 | hasOccurred (intError conditionBranch) = intptr{intError = intError conditionBranch}
 | hasOccurred (intError errIntptr) = errIntptr
 | conditionResult = setResult ifResult ifStatement
 | otherwise = goToNextCondition
 where
   conditionBranch = visit leftLeft intptr
   conditionVisit = getBoolValue (resultValue(fromMaybe Result{resultValue = None, resultPos = Nothing } (currentResult conditionBranch))) intptr leftLeft
   errIntptr = fst conditionVisit
   conditionResult = snd conditionVisit
   ifStatement = visit leftRight intptr
   ifResult = getResultSafe(currentResult ifStatement)
   goToNextCondition =
        if right == Empty
        then intptr
        else visitIfNode right intptr
visitIfConditionNode _ intptr = intptr{intError = throwError (intError intptr) (DeveloperException (intprFileName intptr))}

visitLoopNode :: Node -> Interpreter -> Interpreter
visitLoopNode (Tree (Tree leftLeft _ _ leftRight) _ _ (Tree rightLeft _ _ rightRight)) intptr
  | hasOccurred (intError withVisit) = intptr{intError = intError withVisit}
  | otherwise = runLoopNode fromValue toValue withValue statementNode intptr
  where
    fromVisit = visit leftLeft intptr
    toVisit = visit leftRight fromVisit
    withVisit = visit rightLeft toVisit
    fromValue = fromJust(currentResult fromVisit )
    toValue = fromJust(currentResult toVisit  )
    withValue = fromJust(currentResult withVisit )
    statementNode = rightRight
visitLoopNode (Branch _ _ right) intptr = visit right intptr
visitLoopNode _ intptr = intptr{intError = throwError (intError intptr) (DeveloperException (intprFileName intptr))}


runLoopNode :: Result -> Result -> Result -> Node -> Interpreter -> Interpreter
runLoopNode fromValue toValue withValue statementNode intptr
  | resultValue fromValue == resultValue toValue  = intptr
  | otherwise = runLoopNode newIndex toValue withValue statementNode nextIteration
  where
    newIndex = fromJust (currentResult(addResult fromValue withValue intptr))
    nextIteration = visit statementNode intptr


visitUntilNode :: Node -> Interpreter -> Interpreter
visitUntilNode node@(Tree left _ _ right) intptr
  | hasOccurred (intError conditionVisit) = intptr{intError = intError conditionVisit}
  | snd (getBoolValue (resultValue conditionResult) intptr left) = intptr
  | otherwise = visitUntilNode node nextIteration
  where
    conditionVisit = visit left intptr
    conditionResult = fromJust(currentResult conditionVisit)
    nextIteration = visit right intptr
visitUntilNode _ intptr = intptr{intError = throwError (intError intptr) (DeveloperException (intprFileName intptr))}

visitCreateFunctionNode :: Node -> Interpreter -> Interpreter
visitCreateFunctionNode (Tree l1 _ _ (Tree r1 _ _ r2)) intptr = newEnv
 where
   funcName = show(fromJust(tokenVal (getToken l1)))
   param = visitCreateParameterNode r1
   function_object = Function{functionParameters = param, functionStatement = r2}
   newEnv = intptr {intEnv = setEnvironmentValue (intEnv intptr) funcName (FunctionValue function_object)}
visitCreateFunctionNode _ intptr = intptr{intError = throwError (intError intptr) (DeveloperException (intprFileName intptr))}


visitCreateParameterNode :: Node -> [String]
visitCreateParameterNode (Branch tok _ r1) = show(fromJust(tokenVal tok)) : visitCreateParameterNode r1
visitCreateParameterNode _ = []

visitListNode :: Node -> Interpreter -> Interpreter
visitListNode (Branch _ _ r1) intptr = intptr{ currentResult = Just Result{resultValue = ListValue getValues, resultPos = Nothing}}
  where
    getValues = mapVisit (resultValue . fromJust . currentResult) r1 intptr
visitListNode _ intptr = intptr{intError = throwError (intError intptr) (DeveloperException (intprFileName intptr))}

visitImportNode :: Node -> Interpreter -> IO Interpreter
visitImportNode (Branch _ _ right) intptr = do
  let getPath = visit right intptr
  if hasOccurred (intError getPath) then
    return getPath
    else do
      if isNothing (currentResult getPath) then
        return intptr{intError = throwError (intError intptr) (InvalidInput (intprFileName intptr) "" (tokenPos (getToken right)))}
        else do
          let getPathValue = resultValue (fromJust (currentResult getPath))
          case getPathValue of
            StringValue a -> do 
              newint <- visitImportFile a (tokenPos (getToken right)) getPath
              return newint{currentResult = Nothing }
            a -> return intptr{intError = throwError (intError intptr) (InvalidInput (intprFileName intptr) (show a) (tokenPos (getToken right)))}
visitImportNode _ intptr = return intptr{intError = throwError (intError intptr) (DeveloperException (intprFileName intptr))}



visitPrintNode :: Node -> Interpreter -> Interpreter
visitPrintNode (Branch _ _ right) intptr
  | isNothing (currentResult newIntpr) = newIntpr
  | otherwise = newIntpr {printResultList = printResultList newIntpr ++ [currentResult newIntpr], currentResult = Nothing}
  where
    newIntpr = visit right intptr
visitPrintNode _ intptr = intptr{intError = throwError (intError intptr) (DeveloperException (intprFileName intptr))}


visitImportFile :: String -> Position -> Interpreter -> IO Interpreter
visitImportFile path position intptr = do
  checkFile <- doesFileExist path
  if checkFile
    then do
      handle <- openFile path ReadMode
      content <- hGetContents handle
      runResult intptr (takeFileName path) content
    else return intptr{intError = throwError (intError intptr) (FileNotFound (intprFileName intptr) path position)}


visitRunFunctionNode :: Node -> Interpreter -> Interpreter
visitRunFunctionNode (Tree l1 _ _ r1) intptr = case getEnvironmentValue (intEnv intptr) funcName of
  Just (FunctionValue a) -> visitFunctionStatement (functionStatement a) intptr (visitRunParameters r1 (functionParameters a) intptr)
  Nothing -> intptr{intError = throwError (intError intptr) (NotDefinedError (intprFileName intptr) ( "\"" ++ funcName ++ "\"") (tokenPos (getToken l1)))}
  _ -> intptr{intError = throwError (intError intptr) (InvalidSyntaxError (intprFileName intptr) "function" ( "\"" ++ tokenType (getToken l1) ++ "\"")  (tokenPos (getToken l1)))}
  where
    funcName = show(fromJust(tokenVal (getToken l1)))
visitRunFunctionNode _ intptr = intptr{intError = throwError (intError intptr) (DeveloperException (intprFileName intptr))}

visitRunParameters :: Node -> [String] -> Interpreter -> Interpreter
visitRunParameters parVal parNam intptr = if getNodeLength parVal == length parNam then
  setParameterValue intptr (zipMapNodeTree visit parNam parVal intptr)
  else intptr{intError = throwError (intError intptr) (InvalidNumberOfArguments (intprFileName intptr) (length parNam) (getNodeLength parVal) (tokenPos (getToken parVal)))}

setParameterValue :: Interpreter -> [(String, Result)] -> Interpreter
setParameterValue intptr [] = intptr
setParameterValue intptr [(name, value)] = intptr {intEnv = setEnvironmentValue (intEnv intptr) name (resultValue value)}
setParameterValue intptr ((name, value):xs)  = setParameterValue (intptr {intEnv = setEnvironmentValue (intEnv intptr) name (resultValue value)}) xs

visitFunctionStatement :: Node -> Interpreter -> Interpreter -> Interpreter
visitFunctionStatement stat _ funcIntptr = case currentResult newIntptr of
  (Just _) -> setResult (fromJust (currentResult newIntptr)) newIntptr
  Nothing -> newIntptr
  where
    newIntptr = visit stat funcIntptr

runResult :: Interpreter -> String -> String -> IO Interpreter
runResult inter file_name input = do
  if null input then
    return inter{intError = (Error{hasOccurred = False, errorMessage = []})}
    else do
      let lexer = Lexer {
        fileName = file_name,
        inputText = input ,
        currentLine = 0,
        currentPosition = Position{
                            posIndex = 1,
                            posColumn = 1,
                            posLine = 0},
        currentChar = head input,
        lexerError = Error{hasOccurred = False, errorMessage = []},
        tokenList = []}
      let runLexer = createTokens lexer  
      let parser = Parser {
        tokens = tokenList runLexer,
        tokenIndex = 1,
        currentToken = head (tokenList runLexer),
        currentNode = Empty,
        file = fileName runLexer,
      errorParser = lexerError runLexer}
      let parseTree = parse parser
      let interpreter = inter{intError = errorParser parseTree}
      case getNodeType (currentNode parseTree) of
        ImportNode -> visitImportNode (currentNode parseTree) interpreter
        _ -> return (visit (currentNode parseTree) interpreter)

runInterpreter :: Interpreter -> IO ()
runInterpreter inter = do
  hSetBuffering stdout NoBuffering
  putStr ">> "
  input <- getLine

  let result = runResult inter "Shell" input
  checkResult result
  newIntptr <- result
  runInterpreter newIntptr{currentResult = Nothing, printResultList = []}

checkResult :: IO Interpreter -> IO ()
checkResult inter = do
  checkInterpreter <- inter
  if hasOccurred (intError checkInterpreter)
  then putStrLn (Data.List.intercalate "\n" (errorMessage (intError checkInterpreter)))
  else mapM_ printResult (printResultList checkInterpreter ++ [currentResult checkInterpreter])

printResult :: Maybe Result -> IO ()
printResult a
  | isJust a = print (resultValue (fromJust a))
  | otherwise = putStr ""


