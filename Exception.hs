module Exception where 
import Types

data Error = Error{
  hasOccurred :: Bool,
  errorMessage :: [String]
}  deriving (Show, Eq, Ord) 

data ErrorType = InvalidCharError String String Position |
             InvalidSyntaxError String String String Position |
             DivisionByZeroError String Position |
             NotDefinedError String String Position |
             ConditionError String Position |
             InvalidParameterName String String Position |
             InvalidNumberOfArguments String Int Int Position |
             InvalidOperation String String String String Position |
             OutOfBoundsIndex String String Position 

throwError :: Error -> ErrorType -> Error
throwError (Error occ lst) (InvalidCharError fn err pos) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [(fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " ++ "Error: invalid character: " ++ err)]} 

throwError (Error occ lst) (InvalidSyntaxError fn typ err pos) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [(fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " ++ "Error: expected type: " ++ typ  ++ ". Got: " ++ err)]
} 

throwError (Error occ lst) (DivisionByZeroError fn pos) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [(fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " ++ "Error: division by zero.")]
} 

throwError (Error occ lst) (NotDefinedError fn var pos) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [(fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " ++ "Error: " ++ var ++ " is not defined.")]
} 

throwError (Error occ lst) (ConditionError fn pos) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [(fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " ++ "Error: expected boolean condition.")]
} 

throwError (Error occ lst) (InvalidParameterName fn var pos) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [(fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " ++ "Error: " ++ var ++ " is not a valid parameter name.")]
} 

throwError (Error occ lst) (InvalidNumberOfArguments fn num1 num2 pos) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [(fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " ++ "Error: expected " ++ (show num1) ++ " arguments. Got: " ++ (show num2))]
} 

throwError (Error occ lst) (InvalidOperation fn op1 typ1 typ2 pos) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [(fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " 
                ++ "Error: Invalid operation of " ++ ("\"" ++ op1 ++ "\"")  ++ " between " ++ (show typ1) ++ " and " ++ (show typ2) ++ ".")]
} 
  
throwError (Error occ lst) (OutOfBoundsIndex fn idx pos) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [(fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " ++ "Error: index " ++ (show idx) ++ " is out of bounds.")]
} 
