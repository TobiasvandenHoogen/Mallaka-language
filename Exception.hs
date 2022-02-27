module Exception where 
import Types


data Error = InvalidCharError String String Position |
             InvalidSyntaxError String String String Position |
             DivisionByZeroError String Position |
             NotDefinedError String String Position |
             ConditionError String Position |
             InvalidParameterName String String Position 

throwError :: Error -> a

throwError (InvalidCharError fn err pos) = error ( 
  "\n" ++ fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " ++ "Error: invalid character: " ++ err)

throwError (InvalidSyntaxError fn typ err pos) = error (
 "\n" ++ fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " ++ "Error: expected type: " ++ typ  ++ ". Got: " ++ err)

throwError (DivisionByZeroError fn pos) = error ( 
  "\n" ++ fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " ++ "Error: division by zero.")

throwError (NotDefinedError fn var pos) = error ( 
  "\n" ++ fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " ++ "Error: " ++ var ++ " is not defined.")

throwError (ConditionError fn pos) = error (
 "\n" ++ fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " ++ "Error: expected boolean condition.")

throwError (InvalidParameterName fn var pos) = error ( 
  "\n" ++ fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " ++ "Error: " ++ var ++ " is not a valid parameter name.")
