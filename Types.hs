module Types where 

type Ident = String 

data Value = Int Int |
  Float Float |
  String String 
  deriving (Eq, Show)


data Types = Types{intType :: String, 
        floatType :: String,
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
                 endOfFile = "EOF"}