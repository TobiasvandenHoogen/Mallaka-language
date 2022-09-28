{- 
Module      : Types
Description : The data types and functions which are used by multiple modules (ie Lexer, Parser, Interpreter)
Maintainer  : Tobias van den Hoogen  
-}
module Types where

import Data.Maybe
import Data.Map hiding (null, map, foldr, drop, take, fold)

-- This section defines the data types which are used across multiple files 

-- | The token data type which is created by the lexer and used by the parser 
data Token = Token{
  -- | The token type 
  tokenType :: String,
  -- | The value the token contains 
  val :: Maybe Value,
  -- | The position of the token 
  pos :: Position}
  deriving (Show, Eq, Ord)


-- | Data type that contains the position of a token or statement for error messaging 
data Position = Position {
  -- | The index where 
  index :: Int,
  line :: Int,
  column :: Int}
  deriving (Show, Eq, Ord)

--- | Data type to save the value or the memory location of the 
data Environment = Environment{
  lookupTable :: Map String Value,
  parent :: Maybe Environment
}
  deriving Show




-- | The different token types the Mallaka Language contains 
data Types = Types{
  -- | The integer token
  intType :: String,
  -- | The float token
  floatType :: String,
  -- | The string token 
  stringType :: String,
  -- | The boolean token 
  boolType :: String,
  -- | The function token 
  functionType :: String,
  -- | The list token 
  listType :: String,
  -- | The token for the true value of a boolean 
  trueBool :: String,
  -- | The token for the false value of a boolean 
  falseBool :: String,
  -- | The token for the null value 
  nullType :: String,
  -- | The identifier token 
  identifier :: String,
  -- | The plus operation token 
  plusOperation :: String,
  -- | The minus operation token 
  minusOperation :: String,
  -- | The multiply operation token 
  multiplyOperation :: String,
  -- | The division operation token
  divisionOperation :: String,
  -- | The exponential operation token 
  powerOperation :: String,
  -- | The square root operation token 
  sqrootOperation :: String,
  -- | The variable assign token 
  assignOperation :: String,
  -- | The modulo operation token 
  modOperation :: String,
  -- | The index operation token 
  indexOperation :: String,
  -- | The token which signals the opening of a list 
  openList :: String,
  -- | The token which signals the closing of a list 
  closeList :: String,
  -- | The token which signals the opening of a parenthesis 
  leftParent :: String,
  -- | The token which signals the closing of a parenthesis 
  rightParent :: String,
  -- | The and operation token 
  andOperation :: String,
  -- | The or operation token 
  orOperation :: String,
  -- | The not operation token 
  notOperation :: String,
  -- | The equal comparison token 
  equalOperation :: String,
  -- | The not equal comparison token 
  notEqualOperation :: String,
  -- | The less than operation token 
  lessOperation :: String,
  -- | The greater than operation token 
  greaterOperation :: String,
  -- | The less or equal than operation token 
  lessEqOperation  :: String,
  -- | The greater or equal than operation token 
  greaterEqOperation  :: String,
  -- | The if statement token 
  ifOperation :: String,
  -- | The elif statement token 
  elseIfOperation :: String,
  -- | The else statement token 
  elseOperation :: String,
  -- | The for loop token 
  loopOperation :: String,
  -- | The initial iterator in the for loop token 
  fromLoopOperation :: String,
  -- | The limit of the iterator in the for loop token 
  toLoopOperation :: String,
  -- | The increment (or decrement) of the iterator in the for loop token 
  withLoopOperation :: String,
  -- | The until statement token (opposite of the while loop)
  untilOperation :: String,
  -- | The create function token 
  function :: String,
  -- | The token which signals the opening of the parameter parenthesis 
  openParameter :: String,
  -- | The token which signals the closing of the parameter parenthesis 
  closeParameter :: String,
  -- | The separation of elements token 
  seperatorParameter :: String,
  -- | The function call token 
  runFunction :: String,
  -- | The import of a file token 
  importFunction :: String,
  -- | The print token 
  printFunction :: String,
  -- | The token which signals the opening of a statement
  openStatement :: String,
  -- | The token which signals the closing of a statement 
  closeStatement :: String,
  -- | The newline token 
  newLine :: String,
  -- | The token that separates and signals the end of a statement 
  statementSeperator :: String,
  -- | The end of file token 
  endOfFile :: String}


-- | All the value types in the Mallaka language 
data Value =
  -- | The null type 
  None |
  -- | The integer type 
  Int Int |
  -- | The float type
  Float Float |
  -- | The boolean type 
  Bool Bool |
  -- | The string type 
  String String |
  -- | The function type 
  Func Function |
  -- | The list type 
  List List
  deriving ( Ord, Eq)

-- | Datatype that stores a function 
data Function = Function{
  -- | The variable names of the parameters
  parameters :: [String],
  -- | The statement of the function
  statement :: Node}
  deriving (Ord, Eq, Show)



-- | The list type which consists of a list of values and is used in the Value datatype 
-- | I created this type so it was easier to pattern match in functions 
type List = [Value]


-- | Show overloading so that only the value (and not the datatype) gets printed
instance Show Value where
  -- Print "null" when the value is empty 
  show (None) = "null"
  -- Extract integer value and convert it to a string 
  show (Int a) = show a
  -- Extract float value and convert it to a string 
  show (Float a) = show a
  -- Extract integer value and convert it to a string 
  show (Bool a) = show a
  -- Extract integer value and convert it to a string 
  show (String a) = a
  -- Extract integer value and convert it to a string 
  show (Func a) = show a
  -- Extract integer value and convert it to a string 
  show (List a) = show a

-- | Datatype used to store the result of the parse tree 
data Number = Number{
  -- | The result of a statement 
  numberValue :: Value,
  -- | The position of the statement 
  numPos :: Maybe Position
}
  deriving (Show, Eq)


-- | The node datatype which is used to create a parse tree 
data Node =
  -- | The empty node which contains no token 
  Empty |
  -- | The leaf node that does not expand
  Leaf Token NodeType |
  -- | The branch node that expands to the right side 
  Branch Token NodeType Node |
  -- | The tree node that expands to the left and right side 
  Tree Node Token NodeType Node
  deriving (Show, Ord, Eq)


-- | Datatype used to indicate the different types of the nodes created by the parse tree 
data NodeType =
  -- | The node type is not important 
  NoType |
  -- | The node contains a value 
  NumberNode |
  -- | Variable access node 
  VarAccessNode |
  -- | Variable assign node 
  VarAssignNode |
  -- | Binary operation node 
  BinaryOpNode |
  -- | Unary operation node 
  UnaryNode |
  -- | Index assign node 
  IndexAssignNode |
  -- | Statement separator node 
  SeparatorNode |
  -- | If statement node 
  IfNode |
  -- | For loop node 
  LoopNode |
  -- | Until statement node 
  UntilNode |
  -- | Function creation node 
  FunctionAssignNode |
  -- | Function call node 
  FunctionRunNode |
  -- | List node 
  ListNode |
  -- | Include statement node 
  ImportNode |
  -- | Print function call node 
  PrintNode
  deriving (Show, Ord, Eq)


-- This section defines the functions which are used across multiple files 


getEnvironmentValue :: Environment -> String -> Maybe Value
getEnvironmentValue env name =
  if isNothing returnValue && isJust(parent env)
  then getEnvironmentValue (fromJust(parent env))name
  else returnValue
  where
    returnValue = Data.Map.lookup name (lookupTable env)

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

-- | The defined tokens with its corresponding string value
definedTypes :: Types
definedTypes = Types{
  intType = "integer",
  floatType = "float",
  stringType = "string",
  boolType = "boolean",
  functionType = "function",
  listType = "list",
  trueBool = "True",
  falseBool = "False",
  nullType = "null",
  identifier = "identifier",
  plusOperation = "+",
  minusOperation = "-",
  multiplyOperation = "*",
  divisionOperation = "/",
  modOperation = "%",
  powerOperation = "^",
  sqrootOperation = "carrot",
  assignOperation = "=",
  indexOperation = "$",
  leftParent = "(",
  rightParent = ")",
  openList = "[",
  closeList = "]",
  andOperation = "&",
  orOperation = "|",
  notOperation = "!",
  equalOperation = "==",
  notEqualOperation = "!=",
  lessOperation = "<",
  greaterOperation = ">",
  lessEqOperation = "<=",
  greaterEqOperation = ">=",
  ifOperation = "whatif",
  elseIfOperation = "orwhatif",
  elseOperation = "orelse",
  loopOperation = "loop",
  fromLoopOperation = "from",
  toLoopOperation = "to",
  withLoopOperation = "with",
  untilOperation = "until",
  function = "process",
  openParameter = "{",
  closeParameter = "}",
  seperatorParameter = ",",
  runFunction = "run",
  importFunction = "include",
  printFunction = "see",
  openStatement = "->",
  closeStatement = "<-",
  newLine = "\n",
  statementSeperator = ";",
  endOfFile = "EOF"
}


getToken :: Node -> Token
getToken (Leaf tok _) = tok
getToken (Branch tok _ _) = tok
getToken (Tree _ tok _ _) = tok

getValue :: Node -> Value
getValue Empty = None
getValue (Leaf tok _) = fromJust(val tok)
getValue (Branch tok _ _) = fromJust(val tok)
getValue (Tree _ tok _ _) = fromJust(val tok)

printValueType :: Value -> String
printValueType None = "null"
printValueType (Int a) = intType definedTypes
printValueType (Float a) = floatType definedTypes
printValueType (Bool a) = boolType definedTypes
printValueType (String a) = stringType definedTypes
printValueType (Func a) = functionType definedTypes
printValueType (List a) = listType definedTypes

getNodeType :: Node -> NodeType
getNodeType Empty = NoType
getNodeType (Leaf _ typ) = typ
getNodeType (Branch _ typ _) = typ
getNodeType (Tree _ _ typ _) = typ

getNodeLength :: Node -> Int
getNodeLength Empty = 0
getNodeLength (Leaf _ _ ) = 1
getNodeLength (Branch _ _ r1) = 1 + getNodeLength r1
getNodeLength (Tree l1 _ _ r1) = 1 + getNodeLength r1

getValueSafe :: Maybe Number -> Value
getValueSafe = maybe None numberValue

getNumberSafe :: Maybe Number -> Number
getNumberSafe = fromMaybe Number {numberValue = None, numPos = Nothing}

