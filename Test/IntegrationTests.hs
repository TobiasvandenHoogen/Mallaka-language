module Test.IntegrationTests where 
import Test.Tasty
import Test.Tasty.HUnit
import Source.Lexer
import Source.Parser
import Source.Interpreter
import Source.Exception

import Source.Types

import Data.Map
import Data.Map.Internal.Debug (node)


inputToNode :: String -> Node  
inputToNode input = currentNode parsedInput
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
    
tokenListToResult :: [Token] -> String 
tokenListToResult lst = show (getValueSafe (currentResult result))
  where parser = Parser {
          tokens = lst,
          tokenIndex = 1,
          currentToken = head lst,
          currentNode = Empty,
          file = "test case",
          errorParser =  Error{hasOccurred = False, errorMessage = []}}
        parsedInput = parse parser
        inter = Interpreter {intprFileName = "Test case",
          intEnv = Environment{lookupTable = Data.Map.empty, 
          parent = Nothing},
          currentResult = Nothing,
          printResultList = [],
          intError = Error{hasOccurred = False, errorMessage = []}} 
        result = visit (currentNode parsedInput) inter 

integrationTests = testGroup "Integration Tests" [lexerParserIntTests, parserIntptrIntTests]      

lexerParserIntTests :: TestTree
lexerParserIntTests = testGroup "Lexer to Parser integration tests"
    [ testCase "List addition " $ 
        inputToNode "a = [3, 4, 9, 8]; b = [5, 9, 10]; a + b;" 
      @?=
        Tree 
          (Tree 
            (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "a"), tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}) VarAccessNode) 
            (Token {tokenType = "=", tokenVal = Nothing, tokenPos = Position {posIndex = 3, posLine = 0, posColumn = 1}}) VarAssignNode 
            (Branch (Token {tokenType = "[", tokenVal = Nothing, tokenPos = Position {posIndex = 5, posLine = 0, posColumn = 1}}) ListNode 
              (Tree 
                (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 3), tokenPos = Position {posIndex = 7, posLine = 0, posColumn = 1}}) ValueNode) 
                (Token {tokenType = ",", tokenVal = Nothing, tokenPos = Position {posIndex = 7, posLine = 0, posColumn = 1}}) NoType 
                (Tree 
                  (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 4), tokenPos = Position {posIndex = 10, posLine = 0, posColumn = 1}}) ValueNode) 
                  (Token {tokenType = ",", tokenVal = Nothing, tokenPos = Position {posIndex = 10, posLine = 0, posColumn = 1}}) NoType 
                  (Tree 
                    (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 9), tokenPos = Position {posIndex = 13, posLine = 0, posColumn = 1}}) ValueNode) 
                    (Token {tokenType = ",", tokenVal = Nothing, tokenPos = Position {posIndex = 13, posLine = 0, posColumn = 1}}) NoType 
                    (Tree 
                      (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 8), tokenPos = Position {posIndex = 16, posLine = 0, posColumn = 1}}) ValueNode) 
                      (Token {tokenType = "]", tokenVal = Nothing, tokenPos = Position {posIndex = 16, posLine = 0, posColumn = 1}}) NoType Empty)))))) 
          (Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 17, posLine = 0, posColumn = 1}}) SeparatorNode 
          (Tree 
            (Tree 
              (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "b"), tokenPos = Position {posIndex = 19, posLine = 0, posColumn = 1}}) VarAccessNode) 
              (Token {tokenType = "=", tokenVal = Nothing, tokenPos = Position {posIndex = 21, posLine = 0, posColumn = 1}}) VarAssignNode 
              (Branch (Token {tokenType = "[", tokenVal = Nothing, tokenPos = Position {posIndex = 23, posLine = 0, posColumn = 1}}) ListNode 
                (Tree 
                  (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 5), tokenPos = Position {posIndex = 25, posLine = 0, posColumn = 1}}) ValueNode) 
                  (Token {tokenType = ",", tokenVal = Nothing, tokenPos = Position {posIndex = 25, posLine = 0, posColumn = 1}}) NoType 
                  (Tree   
                    (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 9), tokenPos = Position {posIndex = 28, posLine = 0, posColumn = 1}}) ValueNode) 
                    (Token {tokenType = ",", tokenVal = Nothing, tokenPos = Position {posIndex = 28, posLine = 0, posColumn = 1}}) NoType 
                    (Tree 
                      (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 10), tokenPos = Position {posIndex = 32, posLine = 0, posColumn = 1}}) ValueNode) 
                      (Token {tokenType = "]", tokenVal = Nothing, tokenPos = Position {posIndex = 32, posLine = 0, posColumn = 1}}) NoType Empty))))) 
            (Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 33, posLine = 0, posColumn = 1}}) SeparatorNode 
            (Tree 
              (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "a"), tokenPos = Position {posIndex = 35, posLine = 0, posColumn = 1}}) VarAccessNode) 
              (Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 37, posLine = 0, posColumn = 1}}) BinaryOpNode 
              (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "b"), tokenPos = Position {posIndex = 39, posLine = 0, posColumn = 1}}) VarAccessNode)))
    , testCase "Boolean operation" $ 
        inputToNode "True & (False | True);" 
      @?= 
        Tree 
          (Leaf (Token {tokenType = "True", tokenVal = Just (BoolValue True), tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}) ValueNode) 
          (Token {tokenType = "&", tokenVal = Nothing, tokenPos = Position {posIndex = 6, posLine = 0, posColumn = 1}}) BinaryOpNode 
          (Tree 
            (Leaf (Token {tokenType = "False", tokenVal = Just (BoolValue False), tokenPos = Position {posIndex = 9, posLine = 0, posColumn = 1}}) ValueNode) 
            (Token {tokenType = "|", tokenVal = Nothing, tokenPos = Position {posIndex = 15, posLine = 0, posColumn = 1}}) BinaryOpNode 
            (Leaf (Token {tokenType = "True", tokenVal = Just (BoolValue True), tokenPos = Position {posIndex = 17, posLine = 0, posColumn = 1}}) ValueNode))
    , testCase "Square root operation" $ 
        inputToNode "carrot (80 ^ 10);" 
      @?=   
        Tree 
          Empty 
          (Token {tokenType = "carrot", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}) BinaryOpNode 
          (Tree 
            (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 80), tokenPos = Position {posIndex = 11, posLine = 0, posColumn = 1}}) ValueNode) 
            (Token {tokenType = "^", tokenVal = Nothing, tokenPos = Position {posIndex = 12, posLine = 0, posColumn = 1}}) BinaryOpNode 
            (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 10), tokenPos = Position {posIndex = 16, posLine = 0, posColumn = 1}}) ValueNode))
    ]

parserIntptrIntTests :: TestTree
parserIntptrIntTests = testGroup "Parser to Interpreter Unit tests"
    [ testCase "List addition" $ tokenListToResult
        [Token {tokenType = "identifier", tokenVal = Just (StringValue "c"), tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "=", tokenVal = Nothing, tokenPos = Position {posIndex = 3, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "[", tokenVal = Nothing, tokenPos = Position {posIndex = 5, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "float", tokenVal = Just (FloatValue 9.0), tokenPos = Position {posIndex = 9, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ",", tokenVal = Nothing, tokenPos = Position {posIndex = 9, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "string", tokenVal = Just (StringValue "test"), tokenPos = Position {posIndex = 11, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ",", tokenVal = Nothing, tokenPos = Position {posIndex = 17, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "integer", tokenVal = Just (IntValue 15), tokenPos = Position {posIndex = 21, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ",", tokenVal = Nothing, tokenPos = Position {posIndex = 21, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "string", tokenVal = Just (StringValue "hello"), tokenPos = Position {posIndex = 23, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "]", tokenVal = Nothing, tokenPos = Position {posIndex = 30, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 31, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "identifier", tokenVal = Just (StringValue "b"), tokenPos = Position {posIndex = 33, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "=", tokenVal = Nothing, tokenPos = Position {posIndex = 35, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "[", tokenVal = Nothing, tokenPos = Position {posIndex = 37, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "float", tokenVal = Just (FloatValue 178.0), tokenPos = Position {posIndex = 43, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ",", tokenVal = Nothing, tokenPos = Position {posIndex = 43, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "string", tokenVal = Just (StringValue "world"), tokenPos = Position {posIndex = 45, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "]", tokenVal = Nothing, tokenPos = Position {posIndex = 52, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 53, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "identifier", tokenVal = Just (StringValue "c"), tokenPos = Position {posIndex = 55, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 57, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "identifier", tokenVal = Just (StringValue "b"), tokenPos = Position {posIndex = 59, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 60, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 61, posLine = 0, posColumn = 1}}
        ]
       
      @?= "[9.0,test,15,hello,[178.0,world]]"
    , testCase "Boolean operation" $ tokenListToResult 
      [Token {tokenType = "!", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "False", tokenVal = Just (BoolValue False), tokenPos = Position {posIndex = 2, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "|", tokenVal = Nothing, tokenPos = Position {posIndex = 8, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "(", tokenVal = Nothing, tokenPos = Position {posIndex = 10, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "!", tokenVal = Nothing, tokenPos = Position {posIndex = 11, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "True", tokenVal = Just (BoolValue True), tokenPos = Position {posIndex = 12, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "|", tokenVal = Nothing, tokenPos = Position {posIndex = 17, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "False", tokenVal = Just (BoolValue False), tokenPos = Position {posIndex = 19, posLine = 0, posColumn = 1}}
      ,Token {tokenType = ")", tokenVal = Nothing, tokenPos = Position {posIndex = 24, posLine = 0, posColumn = 1}}
      ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 25, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 26, posLine = 0, posColumn = 1}}
      ]
      @?= "True"
    , testCase "Function implementation" $ tokenListToResult
      [Token {tokenType = "process", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "identifier", tokenVal = Just (StringValue "sum"), tokenPos = Position {posIndex = 9, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "{", tokenVal = Nothing, tokenPos = Position {posIndex = 13, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "identifier", tokenVal = Just (StringValue "a"), tokenPos = Position {posIndex = 14, posLine = 0, posColumn = 1}}
      ,Token {tokenType = ",", tokenVal = Nothing, tokenPos = Position {posIndex = 15, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "identifier", tokenVal = Just (StringValue "b"), tokenPos = Position {posIndex = 17, posLine = 0, posColumn = 1}}
      ,Token {tokenType = ",", tokenVal = Nothing, tokenPos = Position {posIndex = 18, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "identifier", tokenVal = Just (StringValue "c"), tokenPos = Position {posIndex = 20, posLine = 0, posColumn = 1}}
      ,Token {tokenType = ",", tokenVal = Nothing, tokenPos = Position {posIndex = 21, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "identifier", tokenVal = Just (StringValue "d"), tokenPos = Position {posIndex = 23, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "}", tokenVal = Nothing, tokenPos = Position {posIndex = 24, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "->", tokenVal = Nothing, tokenPos = Position {posIndex = 26, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "identifier", tokenVal = Just (StringValue "a"), tokenPos = Position {posIndex = 29, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 31, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "identifier", tokenVal = Just (StringValue "b"), tokenPos = Position {posIndex = 33, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 35, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "identifier", tokenVal = Just (StringValue "c"), tokenPos = Position {posIndex = 37, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 39, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "identifier", tokenVal = Just (StringValue "d"), tokenPos = Position {posIndex = 41, posLine = 0, posColumn = 1}}
      ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 42, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "<-", tokenVal = Nothing, tokenPos = Position {posIndex = 43, posLine = 0, posColumn = 1}}
      ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 45, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "run", tokenVal = Nothing, tokenPos = Position {posIndex = 47, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "identifier", tokenVal = Just (StringValue "sum"), tokenPos = Position {posIndex = 51, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "{", tokenVal = Nothing, tokenPos = Position {posIndex = 55, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 57, posLine = 0, posColumn = 1}}
      ,Token {tokenType = ",", tokenVal = Nothing, tokenPos = Position {posIndex = 57, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "integer", tokenVal = Just (IntValue 2), tokenPos = Position {posIndex = 60, posLine = 0, posColumn = 1}}
      ,Token {tokenType = ",", tokenVal = Nothing, tokenPos = Position {posIndex = 60, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "integer", tokenVal = Just (IntValue 3), tokenPos = Position {posIndex = 63, posLine = 0, posColumn = 1}}
      ,Token {tokenType = ",", tokenVal = Nothing, tokenPos = Position {posIndex = 63, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "integer", tokenVal = Just (IntValue 4), tokenPos = Position {posIndex = 66, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "}", tokenVal = Nothing, tokenPos = Position {posIndex = 66, posLine = 0, posColumn = 1}}
      ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 67, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 69, posLine = 0, posColumn = 1}}
      ]
      @?= "10"
    , testCase "Square root operation" $ tokenListToResult   
        [Token {tokenType = "(", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "carrot", tokenVal = Nothing, tokenPos = Position {posIndex = 2, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "integer", tokenVal = Just (IntValue 100), tokenPos = Position {posIndex = 12, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ")", tokenVal = Nothing, tokenPos = Position {posIndex = 12, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "^", tokenVal = Nothing, tokenPos = Position {posIndex = 14, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "(", tokenVal = Nothing, tokenPos = Position {posIndex = 16, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "carrot", tokenVal = Nothing, tokenPos = Position {posIndex = 17, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "float", tokenVal = Just (FloatValue 0.7), tokenPos = Position {posIndex = 27, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ")", tokenVal = Nothing, tokenPos = Position {posIndex = 27, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 28, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 29, posLine = 0, posColumn = 1}}
        ]
      @?= "6.865308"
    ]