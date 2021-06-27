module Types where 

type Ident = String 

data Value = Int Int |
  Float Float
  deriving (Eq, Show)


data Types = Types{intType :: String, 
        floatType :: String,
        plusOperation :: String,
        minusOperation :: String,
        multiplyOperation :: String,
        divisionOperation :: String,
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
                 plusOperation = "+",
                 minusOperation = "-",
                 multiplyOperation = "*",
                 divisionOperation = "/",
                 leftParent = "(",
                 rightParent = ")",
                 endOfFile = "EOF"}