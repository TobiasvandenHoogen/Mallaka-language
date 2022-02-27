module Types where 

data Position = Position {index :: Int,
  line :: Int,
  column :: Int}
  deriving (Show, Eq, Ord) 

data Token = Token{ tokenType :: String,
        val :: Maybe Value,
        pos :: Position}
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
  FunctionAssignNode
  deriving (Show, Ord, Eq) 

data Node = Empty | Leaf Token NodeType | Branch Token NodeType Node | Tree Node Token NodeType Node
  deriving (Show, Ord, Eq)



-- data Node = Node{
--   leftNode :: Maybe Node,
--   token :: Token,
--   rightNode :: Maybe Node,
--   nodeType :: String}
--   deriving (Show, Ord, Eq)  

type Ident = String 

data Value = Int Int |
  Float Float |
  Bool Bool |
  String String |
  Func Function 
  deriving ( Ord, Eq)

instance Show Value where
  show (Int a) = show a 
  show (Float a) = show a 
  show (Bool a) = show a 
  show (String a) = a 

data Function = Function{
  parameters :: [Value],
  statement :: Node}
  deriving (Ord, Eq)


data Types = Types{intType :: String, 
        floatType :: String,
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
        leftParent :: String,
        rightParent :: String,
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
        returnFunction  :: String,
        openStatement :: String,
        closeStatement :: String,
        endOfFile :: String}
        

definedTypes :: Types 
definedTypes = Types{intType = "integer",
                 floatType = "float",
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
                 leftParent = "(",
                 rightParent = ")",
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
                 returnFunction = "output",
                 openStatement = "->",
                 closeStatement = "<-",
                 endOfFile = "EOF"}

getToken :: Node -> Token
getToken (Leaf tok _) = tok
getToken (Branch tok _ _) = tok
getToken (Tree _ tok _ _) = tok

getNodeType :: Node -> NodeType
getNodeType (Leaf _ typ) = typ
getNodeType (Branch _ typ _) = typ
getNodeType (Tree _ _ typ _) = typ