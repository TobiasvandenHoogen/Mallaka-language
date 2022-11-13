{- 
Module      : Types
Description : The data types and functions which are used by multiple modules (ie Lexer, Parser, Interpreter)
Maintainer  : Tobias van den Hoogen  
-}
{-# LANGUAGE DerivingStrategies #-}

module Source.Types (module Source.Types) where 

import Data.Maybe

-- This section defines the data types for the interpreter which are used across multiple files 


-- | The token data type which is created by the lexer and used by the parser 
data Token = Token 
    { tokenType :: !String              -- ^ The token type 
    , tokenVal :: !(Maybe Value)        -- ^ The value the token contains 
    , tokenPos :: !Position             -- ^ The position of the token within the given input
    } deriving stock (Show, Eq, Ord) 


-- | Data type that contains the position of a token or statement for error messaging 
data Position = Position 
    { posIndex :: !Int                  -- ^ The index of the line input of the token/statement
    , posLine :: !Int                   -- ^ The line number of the of the token/statement
    , posColumn :: !Int                 -- ^ The column of the token/statement
    } deriving stock (Show, Eq, Ord) 


-- | The different token types the Mallaka Language contains 
data Types = Types
    { intType :: !String                -- ^ The integer token
    , floatType :: !String              -- ^ The float token
    , stringType :: !String             -- ^ The string token 
    , boolType :: !String               -- ^ The boolean token 
    , functionType :: !String           -- ^ The function token 
    , listType :: !String               -- ^ The list token 
    , trueBool :: !String               -- ^ The token for the true value of a boolean 
    , falseBool :: !String              -- ^ The token for the false value of a boolean 
    , nullType :: !String               -- ^ The token for the null value 
    , identifier :: !String             -- ^ The token for the null value 
    , plusOperation :: !String          -- ^ The plus operation token 
    , minusOperation :: !String         -- ^ The minus operation token 
    , multiplyOperation :: !String      -- ^ The multiply operation token 
    , divisionOperation :: !String      -- ^ The division operation token
    , powerOperation :: !String         -- ^ The exponential operation token 
    , sqrootOperation :: !String        -- ^ The square root operation token 
    , assignOperation :: !String        -- ^ The variable assign token 
    , modOperation :: !String           -- ^ The modulo operation token 
    , indexOperation :: !String         -- ^ The index operation token 
    , openList :: !String               -- ^ The token which signals the opening of a list 
    , closeList :: !String              -- ^ The token which signals the closing of a list 
    , leftParent :: !String             -- ^ The token which signals the opening of a parenthesis 
    , rightParent :: !String            -- ^ The token which signals the closing of a parenthesis 
    , andOperation :: !String           -- ^ The and operation token 
    , orOperation :: !String            -- ^ The or operation token 
    , notOperation :: !String           -- ^ The not operation token 
    , equalOperation :: !String         -- ^ The equal comparison token 
    , notEqualOperation :: !String      -- ^ The not equal comparison token 
    , lessOperation :: !String          -- ^ The less than operation token 
    , greaterOperation :: !String       -- ^ The greater than operation token 
    , lessEqOperation  :: !String       -- ^ The less or equal than operation token 
    , greaterEqOperation  :: !String    -- ^ The greater or equal than operation token 
    , ifOperation :: !String            -- ^ The if statement token 
    , elseIfOperation :: !String        -- ^ The elif statement token 
    , elseOperation :: !String          -- ^ The else statement token 
    , loopOperation :: !String          -- ^ The for loop token 
    , fromLoopOperation :: !String      -- ^ The initial iterator in the for loop token 
    , toLoopOperation :: !String        -- ^ The limit of the iterator in the for loop token 
    , withLoopOperation :: !String      -- ^ The increment (or decrement) of the iterator in the for loop token 
    , untilOperation :: !String         -- ^ The until statement token (opposite of the while loop)
    , function :: !String               -- ^ The create function token 
    , openParameter :: !String          -- ^ The token which signals the opening of the parameter parenthesis 
    , closeParameter :: !String         -- ^ The token which signals the closing of the parameter parenthesis 
    , separatorParameter :: !String     -- ^ The separation of elements token 
    , runFunction :: !String            -- ^ The function call token 
    , importFunction :: !String         -- ^ The import of a file token 
    , printFunction :: !String          -- ^ The print token 
    , openStatement :: !String          -- ^ The token which signals the opening of a statement
    , closeStatement :: !String         -- ^ The token which signals the closing of a statement 
    , newLine :: !String                -- ^ The newline token 
    , statementSeparator :: !String     -- ^ The token that separates and signals the end of a statement 
    , endOfFile :: !String              -- ^ The end of file token 
    } 


-- | All the value types in the Mallaka language 
data Value  
    = None                              -- ^ The null type 
    | IntValue !Int                     -- ^ The integer type 
    | FloatValue !Float                 -- ^ The float type
    | BoolValue !Bool                   -- ^ The boolean type 
    | StringValue !String               -- ^ The string type 
    | FunctionValue !Function           -- ^ The function type 
    | ListValue !List                   -- ^ The list type 
    deriving stock ( Ord, Eq)


-- | Datatype that stores a function 
data Function = Function
    { functionParameters :: ![String]   -- ^ The variable names of the parameters
    , functionStatement :: !Node        -- ^ The statement of the function
    } deriving stock (Ord, Eq, Show)



{- The list type which consists of a list of values and is used in the Value datatype 
I created this type so it was easier to pattern match in functions
-} 
type List = [Value]


-- | Show overloading so that only the value (and not the datatype) gets printed
instance Show Value where
    show None = "null"
    show (IntValue a) = show a 
    show (FloatValue a) = show a 
    show (BoolValue a) = show a 
    show (StringValue a) = a 
    show (FunctionValue a) = show a
    show (ListValue a) = show a


-- | Datatype used to store the result of the parse tree 
data Result = Result
    { resultValue :: !Value             -- ^ The result of a statement 
    , resultPos :: !(Maybe Position)       -- ^ The position of the statement 
    } deriving stock (Show, Eq)


-- | The node datatype which is used to create a parse tree 
data Node  
    = Empty                             -- ^ The empty node which contains no token 
    | Leaf !Token !NodeType             -- ^ The leaf node that does not expand
    | Branch !Token !NodeType !Node     -- ^ The branch node that expands to the right side 
    | Tree !Node !Token !NodeType !Node -- ^ The tree node that expands to the left and right side 
    deriving stock (Show, Ord, Eq)


-- | Datatype used to indicate the different types of the nodes created by the parse tree 
data NodeType  
    = NoType                            -- ^ The node type is not important 
    | ValueNode                         -- ^ The node contains a value of a datatype
    | VarAccessNode                     -- ^ Variable access node 
    | VarAssignNode                     -- ^ Variable assign node 
    | BinaryOpNode                      -- ^ Binary operation node 
    | UnaryNode                         -- ^ Unary operation node 
    | IndexAssignNode                   -- ^ Index assign node 
    | SeparatorNode                     -- ^ Statement separator node 
    | IfNode                            -- ^ If statement node 
    | LoopNode                          -- ^ For loop node 
    | UntilNode                         -- ^ Until statement node 
    | FunctionAssignNode                -- ^ Function creation node 
    | FunctionRunNode                   -- ^ Function call node 
    | ListNode                          -- ^ List node 
    | ImportNode                        -- ^ Include statement node 
    | PrintNode                         -- ^ Print function call node 
    deriving stock (Show, Ord, Eq) 


-- This section defines the functions which are used across multiple files 
        
 
{- The defined tokens with its corresponding string value
Changing these will also change the syntax of the language 
-}
definedTypes :: Types 
definedTypes = Types
    { intType = "integer"
    , floatType = "float"
    , stringType = "string"
    , boolType = "boolean"
    , functionType = "function"
    , listType = "list"
    , trueBool = "True"
    , falseBool = "False"
    , nullType = "null"
    , identifier = "identifier"
    , plusOperation = "+"
    , minusOperation = "-"
    , multiplyOperation = "*"
    , divisionOperation = "/"
    , modOperation = "%"
    , powerOperation = "^"
    , sqrootOperation = "carrot"
    , assignOperation = "="
    , indexOperation = "$"
    , leftParent = "("
    , rightParent = ")"
    , openList = "["
    , closeList = "]"
    , andOperation = "&"
    , orOperation = "|"
    , notOperation = "!"
    , equalOperation = "=="
    , notEqualOperation = "!="
    , lessOperation = "<"
    , greaterOperation = ">"
    , lessEqOperation = "<="
    , greaterEqOperation = ">="
    , ifOperation = "whatif"
    , elseIfOperation = "orwhatif"
    , elseOperation = "orelse"
    , loopOperation = "loop"
    , fromLoopOperation = "from"
    , toLoopOperation = "to"
    , withLoopOperation = "with"
    , untilOperation = "until"
    , function = "process"
    , openParameter = "{"
    , closeParameter = "}"
    , separatorParameter = ","
    , runFunction = "run"
    , importFunction = "include"
    , printFunction = "see"
    , openStatement = "->"
    , closeStatement = "<-"
    , newLine = "\n"
    , statementSeparator = ";"
    , endOfFile = "EOF"
    }


-- | Prints the syntax value of the Value datatype 
getValueType :: Value -> String 
getValueType None = "null"
getValueType (IntValue _) = intType definedTypes 
getValueType (FloatValue _) = floatType definedTypes
getValueType (BoolValue _) = boolType definedTypes
getValueType (StringValue _) = stringType definedTypes
getValueType (FunctionValue _) = functionType definedTypes
getValueType (ListValue _) = listType definedTypes


{- Extracts token from node 
Do not use this function on an empty node!
-}
getToken :: Node -> Token
getToken Empty = error "Do not use this function on an empty node!"
getToken (Leaf tok _) = tok
getToken (Branch tok _ _) = tok
getToken (Tree _ tok _ _) = tok


{- Extracts token value from node 
Using this function on an empty node will return none. 
-}
getValue :: Node -> Value
getValue Empty = None 
getValue (Leaf tok _) = fromJust(tokenVal tok)
getValue (Branch tok _ _) = fromJust(tokenVal tok)
getValue (Tree _ tok _ _) = fromJust(tokenVal tok)


{- Extracts node type value from node 
Using this function on an empty node will return no type. 
-}
getNodeType :: Node -> NodeType
getNodeType Empty = NoType
getNodeType (Leaf _ typ) = typ
getNodeType (Branch _ typ _) = typ
getNodeType (Tree _ _ typ _) = typ


-- | Returns the node length of the parse tree 
getNodeLength :: Node -> Int
getNodeLength Empty = 0 
getNodeLength (Leaf _ _ ) = 1
getNodeLength (Branch _ _ r1) = 1 + getNodeLength r1
getNodeLength (Tree _ _ _ r1) = 1 + getNodeLength r1


-- | Extracts the value within the result in a safe manner 
getValueSafe :: Maybe Result -> Value 
getValueSafe = maybe None resultValue 


-- | Converts a maybe result to a result 
getResultSafe :: Maybe Result -> Result 
getResultSafe = fromMaybe Result{resultValue = None, resultPos = Nothing}  

