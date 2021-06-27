module Exception where 
import Types


data Error = InvalidCharError String String Position|
             InvalidSyntaxError String String String Position |
             DivisionByZeroError String Position

throwError :: Error -> a

throwError (InvalidCharError fn err pos) = error ( 
  "\n" ++ fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " ++ "Error: invalid character: " ++ err)

throwError (InvalidSyntaxError fn typ err pos) = error (
 "\n" ++ fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " ++ "Error: expected type: " ++ typ  ++ ". Got: " ++ err)

throwError (DivisionByZeroError fn pos) = error ( 
  "\n" ++ fn ++ ":" ++ show(line pos) ++ ":" ++ show(index pos) ++ ": " ++ "Error: division by zero.")