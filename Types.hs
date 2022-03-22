module Types where 

import Data.Maybe


data Token = Token{ tokenType :: String,
        val :: Maybe Value,
        pos :: Position}
        deriving (Show, Eq, Ord) 

data Value = Int Int |
  Float Float |
  Bool Bool |
  String String |
  Func Function |
  List List 
  deriving ( Ord, Eq)

type List = [Value]

data Position = Position {index :: Int,
  line :: Int,
  column :: Int}
  deriving (Show, Eq, Ord) 

data NodeType = 
  NoType |
  NumberNode |
  VarAccessNode |
  VarAssignNode |
  BinaryOpNode |
  UnaryNode |
  IfNode |
  LoopNode |
  UntilNode |
  FunctionAssignNode |
  FunctionRunNode |
  ListNode
  deriving (Show, Ord, Eq) 

data Node = Empty | Leaf Token NodeType | Branch Token NodeType Node | Tree Node Token NodeType Node
  deriving (Show, Ord, Eq)

type Ident = String 


instance Show Value where
  show (Int a) = show a 
  show (Float a) = show a 
  show (Bool a) = show a 
  show (String a) = a 
  show (Func a) = (show a)
  show (List a) = (show a)

data Function = Function{
  parameters :: [String],
  statement :: Node}
  deriving (Ord, Eq, Show)


data Types = Types{intType :: String, 
        floatType :: String,
        stringType :: String,
        boolType :: String,
        functionType :: String,
        listType :: String,
        trueBool :: String,
        falseBool :: String,
        nullType :: String,
        identifier :: String,
        plusOperation :: String,
        minusOperation :: String,
        multiplyOperation :: String,
        divisionOperation :: String,
        powerOperation :: String,
        sqrootOperation :: String, 
        assignOperation :: String,
        modOperation :: String,
        indexOperation :: String,
        openList :: String,
        closeList :: String,
        leftParent :: String,
        rightParent :: String,
        stringComma :: String,
        andOperation :: String,
        orOperation :: String,
        notOperation :: String,
        equalOperation :: String,
        notEqualOperation :: String,
        lessOperation :: String,
        greaterOperation :: String,
        lessEqOperation  :: String,
        greaterEqOperation  :: String,
        separationComma :: String,
        ifOperation :: String,
        elseIfOperation :: String,
        elseOperation :: String,
        loopOperation :: String,
        fromLoopOperation :: String,
        toLoopOperation :: String,
        withLoopOperation :: String,
        untilOperation :: String,
        function :: String,
        openParameter :: String,
        closeParameter :: String,
        seperatorParameter :: String,
        returnFunction  :: String,
        runFunction :: String,
        openStatement :: String,
        closeStatement :: String,
        endOfFile :: String}
        

definedTypes :: Types 
definedTypes = Types{intType = "integer",
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
                 stringComma = "\"",
                 andOperation = "&",
                 orOperation = "|",
                 notOperation = "!",
                 equalOperation = "==",
                 notEqualOperation = "!=",
                 lessOperation = "<",
                 greaterOperation = ">",
                 lessEqOperation = "<=",
                 greaterEqOperation = ">=",
                 separationComma = ",",
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
                 returnFunction = "output",
                 runFunction = "run",
                 openStatement = "->",
                 closeStatement = "<-",
                 endOfFile = "EOF"}

getToken :: Node -> Token
getToken (Leaf tok _) = tok
getToken (Branch tok _ _) = tok
getToken (Tree _ tok _ _) = tok

getValue :: Node -> Value
getValue (Leaf tok _) = fromJust(val tok)
getValue (Branch tok _ _) = fromJust(val tok)
getValue (Tree _ tok _ _) = fromJust(val tok)

printValueType :: Value -> String 
printValueType (Int a) = intType definedTypes
printValueType (Float a) = floatType definedTypes
printValueType (Bool a) = boolType definedTypes
printValueType (String a) = stringType definedTypes
printValueType (Func a) = functionType definedTypes
printValueType (List a) = listType definedTypes

getNodeType :: Node -> NodeType
getNodeType (Leaf _ typ) = typ
getNodeType (Branch _ typ _) = typ
getNodeType (Tree _ _ typ _) = typ

getNodeLength :: Node -> Int
getNodeLength Empty = 0 
getNodeLength (Leaf _ _ ) = 1
getNodeLength (Branch _ _ r1) = 1 + getNodeLength r1
getNodeLength (Tree l1 _ _ r1) = 1 + getNodeLength r1


mapInterpreter :: a -> (a -> b) -> (c -> d) -> [d]
mapInterpreter = undefined 