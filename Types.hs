module Types where 

type Ident = String 

data Value = Int Int |
  Float Float |
  Bool Bool |
  String String 
  deriving ( Ord, Eq, Show)


data Types = Types{intType :: String, 
        floatType :: String,
        trueBool :: String,
        falseBool :: String,
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
        endOfFile :: String}
        
data Position = Position {index :: Int,
  line :: Int,
  column :: Int}
  deriving Show 

data Token = Token{ tokenType :: String,
        val :: Maybe Value,
        pos :: Position}
        deriving Show 

definedTypes :: Types 
definedTypes = Types{intType = "integer",
                 floatType = "float",
                 trueBool = "True",
                 falseBool = "False",
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
                 endOfFile = "EOF"}