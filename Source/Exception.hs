{-# LANGUAGE DerivingStrategies #-}
module Source.Exception(module Source.Exception) where 
import Source.Types

data Error = Error{
  hasOccurred :: Bool,
  errorMessage :: [String]
}  deriving stock (Show, Eq, Ord) 

data ErrorType = InvalidCharError String String Position |
             InvalidSyntaxError String String String Position |
             DivisionByZeroError String Position |
             NotDefinedError String String Position |
             ConditionError String Position |
             InvalidParameterName String String Position |
             InvalidNumberOfArguments String Int Int Position |
             InvalidOperation String String String String Position |
             OutOfBoundsIndex String String Position |
             UnexpectedEndOfFile String Position |
             InvalidInput String String Position |
             FileNotFound String String Position 

throwError :: Error -> ErrorType -> Error
throwError (Error _ lst) (InvalidCharError fn err position) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [fn ++ ":" ++ show(posLine position) ++ ":" ++ show(posIndex position) ++ ": " ++ "Error: invalid character: " ++ err]} 

throwError (Error _ lst) (InvalidSyntaxError fn typ err position) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [fn ++ ":" ++ show(posLine position) ++ ":" ++ show(posIndex position) ++ ": " ++ "Error: expected type: " ++ typ  ++ ". Got: " ++ err]
} 

throwError (Error _ lst) (DivisionByZeroError fn position) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [fn ++ ":" ++ show(posLine position) ++ ":" ++ show(posIndex position) ++ ": " ++ "Error: division by zero."]
} 

throwError (Error _ lst) (NotDefinedError fn var position) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [fn ++ ":" ++ show(posLine position) ++ ":" ++ show(posIndex position) ++ ": " ++ "Error: " ++ var ++ " is not defined."]
} 

throwError (Error _ lst) (ConditionError fn position) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [fn ++ ":" ++ show(posLine position) ++ ":" ++ show(posIndex position) ++ ": " ++ "Error: expected boolean condition."]
} 

throwError (Error _ lst) (InvalidParameterName fn var position) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [fn ++ ":" ++ show(posLine position) ++ ":" ++ show(posIndex position) ++ ": " ++ "Error: " ++ var ++ " is not a valid parameter name."]
} 

throwError (Error _ lst) (InvalidNumberOfArguments fn num1 num2 position) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [fn ++ ":" ++ show(posLine position) ++ ":" ++ show(posIndex position) ++ ": " ++ "Error: expected " ++ show num1 ++ " arguments. Got: " ++ show num2]
} 

throwError (Error _ lst) (InvalidOperation fn op1 typ1 typ2 position) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [fn ++ ":" ++ show(posLine position) ++ ":" ++ show(posIndex position) ++ ": " 
                ++ "Error: Invalid operation of " ++ ("\"" ++ op1 ++ "\"")  ++ " between " ++ show typ1 ++ " and " ++ show typ2 ++ "."]
} 
  
throwError (Error _ lst) (OutOfBoundsIndex fn idx position) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [fn ++ ":" ++ show(posLine position) ++ ":" ++ show(posIndex position) ++ ": " ++ "Error: index " ++ show idx ++ " is out of bounds."]
} 

throwError (Error _ lst) (UnexpectedEndOfFile fn position) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [fn ++ ":" ++ show(posLine position) ++ ":" ++ show(posIndex position) ++ ": " ++ "Error: expected a semicolon."]
} 

throwError (Error _ lst) (InvalidInput fn inpt position) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [fn ++ ":" ++ show(posLine position) ++ ":" ++ show(posIndex position) ++ ": " ++ "Error: input: " ++ ("\"" ++ inpt ++ "\"") ++ " is invalid."]
} 

throwError (Error _ lst) (FileNotFound fn inpt position) = Error{
 hasOccurred = True, 
 errorMessage = lst ++ [fn ++ ":" ++ show(posLine position) ++ ":" ++ show(posIndex position) ++ ": " ++ "Error: file: " ++ ("\"" ++ inpt ++ "\"") ++ " not found."]
} 