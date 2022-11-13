module Test.SystemTests where 
import Test.Tasty
import Test.Tasty.HUnit
import Source.Lexer
import Source.Parser
import Source.Interpreter
import Source.Exception

import Source.Types

import Data.Map
import Data.Map.Internal.Debug (node)




inputToResult :: String -> String 
inputToResult input = show (getValueSafe (currentResult result))
  where lexer = Lexer{
          fileName = "test case",
          inputText = input,
          currentLine = 0,
          currentPosition = Position{
                              posIndex = 1,
                              posColumn = 1,
                              posLine = 0},
          currentChar = head input,
          lexerError = Error{hasOccurred = False, errorMessage = []},
          tokenList = []}
        tokenizeInput = createTokens lexer 
        parser = Parser {
          tokens = tokenList tokenizeInput,
          tokenIndex = 1,
          currentToken = head (tokenList tokenizeInput),
          currentNode = Empty,
          file = fileName tokenizeInput, 
          errorParser = lexerError tokenizeInput}
        parsedInput = parse parser 
        inter = Interpreter {intprFileName = "Test case",
          intEnv = Environment{lookupTable = Data.Map.empty, 
          parent = Nothing},
          currentResult = Nothing,
          printResultList = [],
          intError = Error{hasOccurred = False, errorMessage = []}} 
        result = visit (currentNode parsedInput) inter 

systemTests :: TestTree
systemTests = testGroup "System Tests"  
    [ 
      testCase "Average Value List" $ inputToResult 
       "list = [30, 50, 80, 40, 70];\
        \sum = 0;\
        \length = carrot list;\
        \index = 0;\
        \loop from 0 to length with 1 -> sum = sum + (list $ index); index = index + 1;<-;\
        \result = sum / length;\
        \result "
      @?= "54"
    , testCase "4 Nor gate input" $ inputToResult 
       "process Nor {a, b} ->!(a & b);<-;\
       \process fourNor {a, b, c, d} -> e = run Nor {a, b};\ 
                                        \f = run Nor {c, d};\  
                                        \g = run Nor {e, e};\ 
                                        \h = run Nor {f, f};\
                                        \run Nor {g, h};<-;\
       \run fourNor {True, False, False, True};"
      @?= "True"

    ]    