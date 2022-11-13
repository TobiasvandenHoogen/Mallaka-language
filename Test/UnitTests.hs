module Test.UnitTests where 
import Test.Tasty
import Test.Tasty.HUnit
import Source.Lexer
import Source.Parser
import Source.Interpreter
import Source.Exception

import Source.Types

import Data.Map

createLexer :: String -> Lexer 
createLexer input  = Lexer{
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

createParser :: [Token] -> Parser
createParser lst = 
    Parser {
        tokens = lst,
        tokenIndex = 1,
        currentToken = head lst,
        currentNode = Empty,
        file = "test case",
      errorParser =  Error{hasOccurred = False, errorMessage = []}}

createInterpreter :: Interpreter 
createInterpreter = Interpreter {intprFileName = "Test case",
    intEnv = Environment{lookupTable = Data.Map.empty, 
    parent = Nothing},
    currentResult = Nothing,
    printResultList = [],
    intError = Error{hasOccurred = False, errorMessage = []}}
-- | Unit tests 
unitTests :: TestTree
unitTests = testGroup "Unit Tests" [lexerUnitTests, parserUnitTests, interpreterUnitTests]

-- | Checks if the lexer produces the correct token list 
lexerUnitTests :: TestTree
lexerUnitTests = testGroup "Lexer Unit tests"
  [ testCase "Variable assignment" $
      tokenList (createTokens ( createLexer "a = 10;")) @?= 
        [Token {tokenType = "identifier", tokenVal = Just (StringValue "a"), tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "=", tokenVal = Nothing, tokenPos = Position {posIndex = 3, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "integer", tokenVal = Just (IntValue 10), tokenPos = Position {posIndex = 7, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 7, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 8, posLine = 0, posColumn = 1}}
        ]
  , testCase "Addition operation" $
      tokenList (createTokens ( createLexer "749.0 + 87;" )) @?= 
        [Token {tokenType = "float", tokenVal = Just (FloatValue 749.0), tokenPos = Position {posIndex = 6, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 7, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "integer", tokenVal = Just (IntValue 87), tokenPos = Position {posIndex = 11, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 11, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 12, posLine = 0, posColumn = 1}}
        ]
  , testCase "Minus operation" $
      tokenList (createTokens ( createLexer "32.0 - 875;")) @?= 
        [Token {tokenType = "float", tokenVal = Just (FloatValue 32.0), tokenPos = Position {posIndex = 5, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "-", tokenVal = Nothing, tokenPos = Position {posIndex = 6, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "integer", tokenVal = Just (IntValue 875), tokenPos = Position {posIndex = 11, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 11, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 12, posLine = 0, posColumn = 1}}
        ]
  , testCase "Square root operation" $
      tokenList (createTokens ( createLexer "carrot 5625;")) @?= 
        [Token {tokenType = "carrot", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "integer", tokenVal = Just (IntValue 5625), tokenPos = Position {posIndex = 12, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 12, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 13, posLine = 0, posColumn = 1}}
        ]
  , testCase "Parenthesizes" $
      tokenList (createTokens ( createLexer "1000 - 5.0 + (3 * 8.0 ^ 5);")) @?= 
        [Token {tokenType = "integer", tokenVal = Just (IntValue 1000), tokenPos = Position {posIndex = 5, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "-", tokenVal = Nothing, tokenPos = Position {posIndex = 6, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "float", tokenVal = Just  (FloatValue 5.0), tokenPos = Position {posIndex = 11, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 12, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "(", tokenVal = Nothing, tokenPos = Position {posIndex = 14, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "integer", tokenVal = Just (IntValue 3), tokenPos = Position {posIndex = 16, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "*", tokenVal = Nothing, tokenPos = Position {posIndex = 17, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "float", tokenVal = Just  (FloatValue 8.0), tokenPos = Position {posIndex = 22, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "^", tokenVal = Nothing, tokenPos = Position {posIndex = 23, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "integer", tokenVal = Just (IntValue 5), tokenPos = Position {posIndex = 26, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ")", tokenVal = Nothing, tokenPos = Position {posIndex = 26, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 27, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 28, posLine = 0, posColumn = 1}}
        ]
  , testCase "If statement" $
      tokenList (createTokens ( createLexer "whatif(!(8 == 8.0)) ->0;<- orelse->1;<-;")) @?=    
        [Token {tokenType = "whatif", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "(", tokenVal = Nothing, tokenPos = Position {posIndex = 7, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "!", tokenVal = Nothing, tokenPos = Position {posIndex = 8, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "(", tokenVal = Nothing, tokenPos = Position {posIndex = 9, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "integer", tokenVal = Just (IntValue 8), tokenPos = Position {posIndex = 11, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "==", tokenVal = Nothing, tokenPos = Position {posIndex = 12, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "float", tokenVal = Just (FloatValue 8.0), tokenPos = Position {posIndex = 18, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ")", tokenVal = Nothing, tokenPos = Position {posIndex = 18, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ")", tokenVal = Nothing, tokenPos = Position {posIndex = 19, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "->", tokenVal = Nothing, tokenPos = Position {posIndex = 21, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "integer", tokenVal = Just (IntValue 0), tokenPos = Position {posIndex = 24, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 24, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "<-", tokenVal = Nothing, tokenPos = Position {posIndex = 25, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "orelse", tokenVal = Nothing, tokenPos = Position {posIndex = 28, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "->", tokenVal = Nothing, tokenPos = Position {posIndex = 34, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 37, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 37, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "<-", tokenVal = Nothing, tokenPos = Position {posIndex = 38, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 40, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 41, posLine = 0, posColumn = 1}}
        ]
  , testCase "Until loop" $
    tokenList (createTokens ( createLexer "until(a == 100)-> a = a + 1; <-;")) @?=  
      [Token {tokenType = "until", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "(", tokenVal = Nothing, tokenPos = Position {posIndex = 6, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "identifier", tokenVal = Just (StringValue "a"), tokenPos = Position {posIndex = 7, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "==", tokenVal = Nothing, tokenPos = Position {posIndex = 9, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "integer", tokenVal = Just (IntValue 100), tokenPos = Position {posIndex = 15, posLine = 0, posColumn = 1}}
      ,Token {tokenType = ")", tokenVal = Nothing, tokenPos = Position {posIndex = 15, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "->", tokenVal = Nothing, tokenPos = Position {posIndex = 16, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "identifier", tokenVal = Just (StringValue "a"), tokenPos = Position {posIndex = 19, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "=", tokenVal = Nothing, tokenPos = Position {posIndex = 21, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "identifier", tokenVal = Just (StringValue "a"), tokenPos = Position {posIndex = 23, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 25, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 28, posLine = 0, posColumn = 1}}
      ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 28, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "<-", tokenVal = Nothing, tokenPos = Position {posIndex = 30, posLine = 0, posColumn = 1}}
      ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 32, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 33, posLine = 0, posColumn = 1}}
      ] 
  , testCase "For loop" $
      tokenList (createTokens (createLexer "loop from 0 to 100 with 1 ->a = a + 1;<-;")) @?=  
        [Token {tokenType = "loop", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "from", tokenVal = Nothing, tokenPos = Position {posIndex = 6, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "integer", tokenVal = Just (IntValue 0), tokenPos = Position {posIndex = 12, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "to", tokenVal = Nothing, tokenPos = Position {posIndex = 13, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "integer", tokenVal = Just (IntValue 100), tokenPos = Position {posIndex = 19, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "with", tokenVal = Nothing, tokenPos = Position {posIndex = 20, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 26, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "->", tokenVal = Nothing, tokenPos = Position {posIndex = 27, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "identifier", tokenVal = Just (StringValue "a"), tokenPos = Position {posIndex = 29, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "=", tokenVal = Nothing, tokenPos = Position {posIndex = 31, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "identifier", tokenVal = Just (StringValue "a"), tokenPos = Position {posIndex = 33, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 35, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 38, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 38, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "<-", tokenVal = Nothing, tokenPos = Position {posIndex = 39, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 41, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 42, posLine = 0, posColumn = 1}}
        ]
  , testCase "Create function" $
      tokenList (createTokens (createLexer "process addNum {a, b} ->a + b;<-;")) @?=  
      [Token {tokenType = "process", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "identifier", tokenVal = Just (StringValue "addNum"), tokenPos = Position {posIndex = 9, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "{", tokenVal = Nothing, tokenPos = Position {posIndex = 16, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "identifier", tokenVal = Just (StringValue "a"), tokenPos = Position {posIndex = 17, posLine = 0, posColumn = 1}}
      ,Token {tokenType = ",", tokenVal = Nothing, tokenPos = Position {posIndex = 18, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "identifier", tokenVal = Just (StringValue "b"), tokenPos = Position {posIndex = 20, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "}", tokenVal = Nothing, tokenPos = Position {posIndex = 21, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "->", tokenVal = Nothing, tokenPos = Position {posIndex = 23, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "identifier", tokenVal = Just (StringValue "a"), tokenPos = Position {posIndex = 25, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 27, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "identifier", tokenVal = Just (StringValue "b"), tokenPos = Position {posIndex = 29, posLine = 0, posColumn = 1}}
      ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 30, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "<-", tokenVal = Nothing, tokenPos = Position {posIndex = 31, posLine = 0, posColumn = 1}}
      ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 33, posLine = 0, posColumn = 1}}
      ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 34, posLine = 0, posColumn = 1}}
      ]
  , testCase "Call function" $
      tokenList (createTokens (createLexer "run customFunction{13, 87, False};")) @?=  
        [Token {tokenType = "run", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "identifier", tokenVal = Just (StringValue "customFunction"), tokenPos = Position {posIndex = 5, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "{", tokenVal = Nothing, tokenPos = Position {posIndex = 19, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "integer", tokenVal = Just (IntValue 13), tokenPos = Position {posIndex = 22, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ",", tokenVal = Nothing, tokenPos = Position {posIndex = 22, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "integer", tokenVal = Just (IntValue 87), tokenPos = Position {posIndex = 26, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ",", tokenVal = Nothing, tokenPos = Position {posIndex = 26, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "False", tokenVal = Just (BoolValue False), tokenPos = Position {posIndex = 28, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "}", tokenVal = Nothing, tokenPos = Position {posIndex = 33, posLine = 0, posColumn = 1}}
        ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 34, posLine = 0, posColumn = 1}}
        ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 35, posLine = 0, posColumn = 1}}
        ]

  ]

-- | Checks if the parser produces the correct parse tree 
parserUnitTests :: TestTree
parserUnitTests = testGroup "Parser Unit tests"
    [ testCase "Variable assignment" $
        currentNode (parse (createParser 
          [Token {tokenType = "identifier", tokenVal = Just (StringValue "b"), tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "=", tokenVal = Nothing, tokenPos = Position {posIndex = 3, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "float", tokenVal = Just (FloatValue 8.6736464e7), tokenPos = Position {posIndex = 15, posLine = 0
          , posColumn = 1}},Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 15, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 17, posLine = 0, posColumn = 1}}
          ]  
        ))
        @?=
          Tree 
            (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "b"), tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}) VarAccessNode) 
            (Token {tokenType = "=", tokenVal = Nothing, tokenPos = Position {posIndex = 3, posLine = 0, posColumn = 1}}) VarAssignNode 
            (Leaf (Token {tokenType = "float", tokenVal = Just (FloatValue 8.6736464e7), tokenPos = Position {posIndex = 15, posLine = 0, posColumn = 1}}) ValueNode)  
    , testCase "Addition operation" $
        currentNode (parse (createParser 
          [Token {tokenType = "-", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "integer", tokenVal = Just (IntValue 34), tokenPos = Position {posIndex = 4, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 5, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "integer", tokenVal = Just  (IntValue 85), tokenPos = Position {posIndex = 9, posLine = 0, posColumn = 1}}
          ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 9, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 10, posLine = 0, posColumn = 1}}
          ]
        ))
        @?=
          Tree 
            (Branch (Token {tokenType = "-", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}) UnaryNode 
              (Leaf (Token {tokenType = "integer", tokenVal = Just  (IntValue 34), tokenPos = Position {posIndex = 4, posLine = 0, posColumn = 1}}) ValueNode)) 
            (Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 5, posLine = 0, posColumn = 1}}) BinaryOpNode 
            (Leaf (Token {tokenType = "integer", tokenVal = Just  (IntValue 85), tokenPos = Position {posIndex = 9, posLine = 0, posColumn = 1}}) ValueNode)
    , testCase "Minus operation" $
        currentNode (parse (createParser 
          [Token {tokenType = "integer", tokenVal = Just  (IntValue 50), tokenPos = Position {posIndex = 3, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "-", tokenVal = Nothing, tokenPos = Position {posIndex = 4, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "integer", tokenVal = Just  (IntValue 85), tokenPos = Position {posIndex = 8, posLine = 0, posColumn = 1}}
          ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 8, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 9, posLine = 0, posColumn = 1}}
          ]
        ))
        @?=
          Tree 
            (Leaf (Token {tokenType = "integer", tokenVal = Just  (IntValue 50), tokenPos = Position {posIndex = 3, posLine = 0, posColumn = 1}}) ValueNode) 
            (Token {tokenType = "-", tokenVal = Nothing, tokenPos = Position {posIndex = 4, posLine = 0, posColumn = 1}}) BinaryOpNode 
            (Leaf (Token {tokenType = "integer", tokenVal = Just  (IntValue 85), tokenPos = Position {posIndex = 8, posLine = 0, posColumn = 1}}) ValueNode)
    , testCase "Division operation" $
         currentNode (parse (createParser 
          [Token {tokenType = "integer", tokenVal = Just  (IntValue 10), tokenPos = Position {posIndex = 3, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "/", tokenVal = Nothing, tokenPos = Position {posIndex = 4, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "-", tokenVal = Nothing, tokenPos = Position {posIndex = 6, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "integer", tokenVal = Just  (IntValue 5), tokenPos = Position {posIndex = 8, posLine = 0, posColumn = 1}}
          ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 8, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 9, posLine = 0, posColumn = 1}}
          ]
        ))
        @?=
          Tree 
            (Leaf (Token {tokenType = "integer", tokenVal = Just  (IntValue 10), tokenPos = Position {posIndex = 3, posLine = 0, posColumn = 1}}) ValueNode) 
            (Token {tokenType = "/", tokenVal = Nothing, tokenPos = Position {posIndex = 4, posLine = 0, posColumn = 1}}) BinaryOpNode 
            (Branch (Token {tokenType = "-", tokenVal = Nothing, tokenPos = Position {posIndex = 6, posLine = 0, posColumn = 1}}) UnaryNode 
              (Leaf (Token {tokenType = "integer", tokenVal = Just  (IntValue 5), tokenPos = Position {posIndex = 8, posLine = 0, posColumn = 1}}) ValueNode))
    , testCase "Parenthesizes" $
         currentNode (parse (createParser 
          [Token {tokenType = "(", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "integer", tokenVal = Just  (IntValue 8), tokenPos = Position {posIndex = 3, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "-", tokenVal = Nothing, tokenPos = Position {posIndex = 4, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "integer", tokenVal = Just  (IntValue 9), tokenPos = Position {posIndex = 7, posLine = 0, posColumn = 1}}
          ,Token {tokenType = ")", tokenVal = Nothing, tokenPos = Position {posIndex = 7, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 9, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "integer", tokenVal = Just  (IntValue 13), tokenPos = Position {posIndex = 13, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "^", tokenVal = Nothing, tokenPos = Position {posIndex = 14, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "integer", tokenVal = Just  (IntValue 2), tokenPos = Position {posIndex = 17, posLine = 0, posColumn = 1}}
          ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 17, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 18, posLine = 0, posColumn = 1}}
          ]
        ))
        @?=
          Tree 
            (Tree 
              (Leaf (Token {tokenType = "integer", tokenVal = Just  (IntValue 8), tokenPos = Position {posIndex = 3, posLine = 0, posColumn = 1}}) ValueNode) 
              (Token {tokenType = "-", tokenVal = Nothing, tokenPos = Position {posIndex = 4, posLine = 0, posColumn = 1}}) BinaryOpNode 
              (Leaf (Token {tokenType = "integer", tokenVal = Just  (IntValue 9), tokenPos = Position {posIndex = 7, posLine = 0, posColumn = 1}}) ValueNode)) 
            (Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 9, posLine = 0, posColumn = 1}}) BinaryOpNode 
            (Tree 
              (Leaf (Token {tokenType = "integer", tokenVal = Just  (IntValue 13), tokenPos = Position {posIndex = 13, posLine = 0, posColumn = 1}}) ValueNode) 
              (Token {tokenType = "^", tokenVal = Nothing, tokenPos = Position {posIndex = 14, posLine = 0, posColumn = 1}}) BinaryOpNode 
              (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 2), tokenPos = Position {posIndex = 17, posLine = 0, posColumn = 1}}) ValueNode))
    , testCase "If statement" $
         currentNode (parse (createParser 
            [Token {tokenType = "whatif", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "(", tokenVal = Nothing, tokenPos = Position {posIndex = 8, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 10, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "==", tokenVal = Nothing, tokenPos = Position {posIndex = 11, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "integer", tokenVal = Just (IntValue 2), tokenPos = Position {posIndex = 15, posLine = 0, posColumn = 1}}
            ,Token {tokenType = ")", tokenVal = Nothing, tokenPos = Position {posIndex = 15, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "->", tokenVal = Nothing, tokenPos = Position {posIndex = 17, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 20, posLine = 0, posColumn = 1}}
            ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 20, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "<-", tokenVal = Nothing, tokenPos = Position {posIndex = 21, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "orwhatif", tokenVal = Nothing, tokenPos = Position {posIndex = 24, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "(", tokenVal = Nothing, tokenPos = Position {posIndex = 33, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 35, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "==", tokenVal = Nothing, tokenPos = Position {posIndex = 36, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 40, posLine = 0, posColumn = 1}}
            ,Token {tokenType = ")", tokenVal = Nothing, tokenPos = Position {posIndex = 40, posLine = 0, posColumn = 1}
            },Token {tokenType = "->", tokenVal = Nothing, tokenPos = Position {posIndex = 42, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "integer", tokenVal = Just (IntValue 2), tokenPos = Position {posIndex = 45, posLine = 0, posColumn = 1}}
            ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 45, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "<-", tokenVal = Nothing, tokenPos = Position {posIndex = 46, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "orelse", tokenVal = Nothing, tokenPos = Position {posIndex = 49, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "->", tokenVal = Nothing, tokenPos = Position {posIndex = 56, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "integer", tokenVal = Just (IntValue 3), tokenPos = Position {posIndex = 59, posLine = 0, posColumn = 1}}
            ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 59, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "<-", tokenVal = Nothing, tokenPos = Position {posIndex = 60, posLine = 0, posColumn = 1}}
            ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 62, posLine = 0, posColumn = 1}}
            ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 63, posLine = 0, posColumn = 1}}
            ]
          ))
          @?=
          Tree 
            (Tree 
              (Tree 
                (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 10, posLine = 0, posColumn = 1}}) ValueNode) 
                (Token {tokenType = "==", tokenVal = Nothing, tokenPos = Position {posIndex = 11, posLine = 0, posColumn = 1}}) BinaryOpNode 
                (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 2), tokenPos = Position {posIndex = 15, posLine = 0, posColumn = 1}}) ValueNode)) 
              (Token {tokenType = "whatif", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}) NoType 
              (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 20, posLine = 0, posColumn = 1}}) ValueNode)) 
            (Token {tokenType = "whatif", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}) IfNode 
            (Tree 
              (Tree 
                (Tree 
                  (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 35, posLine = 0, posColumn = 1}}) ValueNode) 
                  (Token {tokenType = "==", tokenVal = Nothing, tokenPos = Position {posIndex = 36, posLine = 0, posColumn = 1}}) BinaryOpNode 
                  (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 40, posLine = 0, posColumn = 1}}) ValueNode)) 
                (Token {tokenType = "orwhatif", tokenVal = Nothing, tokenPos = Position {posIndex = 24, posLine = 0, posColumn = 1}}) NoType 
                (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 2), tokenPos = Position {posIndex = 45, posLine = 0, posColumn = 1}}) ValueNode)) 
              (Token {tokenType = "orwhatif", tokenVal = Nothing, tokenPos = Position {posIndex = 24, posLine = 0, posColumn = 1}}) IfNode 
              (Tree 
                (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 3), tokenPos = Position {posIndex = 59, posLine = 0, posColumn = 1}}) ValueNode) 
                (Token {tokenType = "orelse", tokenVal = Nothing, tokenPos = Position {posIndex = 49, posLine = 0, posColumn = 1}}) NoType 
                Empty))
    , testCase "Until loop" $
         currentNode (parse (createParser 
          [Token {tokenType = "until", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "(", tokenVal = Nothing, tokenPos = Position {posIndex = 7, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "identifier", tokenVal = Just (StringValue "b"), tokenPos = Position {posIndex = 8, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "/", tokenVal = Nothing, tokenPos = Position {posIndex = 10, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "integer", tokenVal = Just (IntValue 2), tokenPos = Position {posIndex = 13, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "==", tokenVal = Nothing, tokenPos = Position {posIndex = 14, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "integer", tokenVal = Just  (IntValue 5), tokenPos = Position {posIndex = 18, posLine = 0, posColumn = 1}}
          ,Token {tokenType = ")", tokenVal = Nothing, tokenPos = Position {posIndex = 18, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "->", tokenVal = Nothing, tokenPos = Position {posIndex = 20, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "identifier", tokenVal = Just (StringValue "b"), tokenPos = Position {posIndex = 23, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "=", tokenVal = Nothing, tokenPos = Position {posIndex = 25, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "identifier", tokenVal = Just (StringValue "b"), tokenPos = Position {posIndex = 27, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "-", tokenVal = Nothing, tokenPos = Position {posIndex = 29, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 32, posLine = 0, posColumn = 1}}
          ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 32, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "<-", tokenVal = Nothing, tokenPos = Position {posIndex = 33, posLine = 0, posColumn = 1}}
          ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 35, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 36, posLine = 0, posColumn = 1}}
          ]
        ))
        @?=
          Tree 
            (Tree 
              (Tree 
                (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "b"), tokenPos = Position {posIndex = 8, posLine = 0, posColumn = 1}}) VarAccessNode) 
                (Token {tokenType = "/", tokenVal = Nothing, tokenPos = Position {posIndex = 10, posLine = 0, posColumn = 1}}) BinaryOpNode 
                (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 2), tokenPos = Position {posIndex = 13, posLine = 0, posColumn = 1}}) ValueNode)) 
              (Token {tokenType = "==", tokenVal = Nothing, tokenPos = Position {posIndex = 14, posLine = 0, posColumn = 1}}) BinaryOpNode 
              (Leaf (Token {tokenType = "integer", tokenVal = Just  (IntValue 5), tokenPos = Position {posIndex = 18, posLine = 0, posColumn = 1}}) ValueNode)) 
            (Token {tokenType = "until", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}) UntilNode 
            (Tree 
              (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "b"), tokenPos = Position {posIndex = 23, posLine = 0, posColumn = 1}}) VarAccessNode) 
              (Token {tokenType = "=", tokenVal = Nothing, tokenPos = Position {posIndex = 25, posLine = 0, posColumn = 1}}) VarAssignNode 
              (Tree 
                (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "b"), tokenPos = Position {posIndex = 27, posLine = 0, posColumn = 1}}) VarAccessNode) 
                (Token {tokenType = "-", tokenVal = Nothing, tokenPos = Position {posIndex = 29, posLine = 0, posColumn = 1}}) BinaryOpNode 
                (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 32, posLine = 0, posColumn = 1}}) ValueNode)))
    , testCase "Create function" $
         currentNode (parse (createParser 
         [Token {tokenType = "process", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}
         ,Token {tokenType = "identifier", tokenVal = Just (StringValue "sqrRoot"), tokenPos = Position {posIndex = 9, posLine = 0, posColumn = 1}}
         ,Token {tokenType = "{", tokenVal = Nothing, tokenPos = Position {posIndex = 17, posLine = 0, posColumn = 1}}
         ,Token {tokenType = "identifier", tokenVal = Just (StringValue "num"), tokenPos = Position {posIndex = 18, posLine = 0, posColumn = 1}}
         ,Token {tokenType = "}", tokenVal = Nothing, tokenPos = Position {posIndex = 21, posLine = 0, posColumn = 1}}
         ,Token {tokenType = "->", tokenVal = Nothing, tokenPos = Position {posIndex = 23, posLine = 0, posColumn = 1}}
         ,Token {tokenType = "carrot", tokenVal = Nothing, tokenPos = Position {posIndex = 25, posLine = 0, posColumn = 1}}
         ,Token {tokenType = "identifier", tokenVal = Just (StringValue "num"), tokenPos = Position {posIndex = 32, posLine = 0, posColumn = 1}}
         ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 35, posLine = 0, posColumn = 1}}
         ,Token {tokenType = "<-", tokenVal = Nothing, tokenPos = Position {posIndex = 36, posLine = 0, posColumn = 1}}
         ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 38, posLine = 0, posColumn = 1}}
         ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 39, posLine = 0, posColumn = 1}}
         ]
        ))
        @?=
          Tree 
            (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "sqrRoot"), tokenPos = Position {posIndex = 9, posLine = 0, posColumn = 1}}) NoType) 
            (Token {tokenType = "process", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}) FunctionAssignNode 
            (Tree 
              (Branch (Token {tokenType = "identifier", tokenVal = Just (StringValue "num"), tokenPos = Position {posIndex = 18, posLine = 0, posColumn = 1}}) NoType 
                Empty) 
              (Token {tokenType = "{", tokenVal = Nothing, tokenPos = Position {posIndex = 17, posLine = 0, posColumn = 1}}) NoType 
              (Tree 
                Empty 
                (Token {tokenType = "carrot", tokenVal = Nothing, tokenPos = Position {posIndex = 25, posLine = 0, posColumn = 1}}) BinaryOpNode 
                (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "num"), tokenPos = Position {posIndex = 32, posLine = 0, posColumn = 1}}) VarAccessNode)))
    , testCase "Call function" $
         currentNode (parse (createParser 
          [Token {tokenType = "run", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "identifier", tokenVal = Just (StringValue "voidFunc"), tokenPos = Position {posIndex = 5, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "{", tokenVal = Nothing, tokenPos = Position {posIndex = 14, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "}", tokenVal = Nothing, tokenPos = Position {posIndex = 15, posLine = 0, posColumn = 1}}
          ,Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 16, posLine = 0, posColumn = 1}}
          ,Token {tokenType = "EOF", tokenVal = Nothing, tokenPos = Position {posIndex = 17, posLine = 0, posColumn = 1}}
          ]
      ))
      @?=
        Tree 
          (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "voidFunc"), tokenPos = Position {posIndex = 5, posLine = 0, posColumn = 1}}) NoType) 
          (Token {tokenType = "run", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}) FunctionRunNode 
          Empty
    ]

-- | Checks if the interpreter produces the correct output 
interpreterUnitTests :: TestTree
interpreterUnitTests = testGroup "Interpreter Unit tests"
    [ testCase "Variable assignment" $
        show (getValueSafe (currentResult (visit(
          Tree 
            (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "c"), tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}) VarAccessNode) 
            (Token {tokenType = "=", tokenVal = Nothing, tokenPos = Position {posIndex = 3, posLine = 0, posColumn = 1}}) VarAssignNode 
            (Branch (Token {tokenType = "-", tokenVal = Nothing, tokenPos = Position {posIndex = 5, posLine = 0, posColumn = 1}}) UnaryNode 
              (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 76), tokenPos = Position {posIndex = 8, posLine = 0, posColumn = 1}}) ValueNode))
        )createInterpreter)))
      @?= "-76"
    , testCase "Addition operation" $
      show (getValueSafe (currentResult (visit(
          Tree 
            (Leaf (Token {tokenType = "float", tokenVal = Just (FloatValue 78.0), tokenPos = Position {posIndex = 5, posLine = 0, posColumn = 1}}) ValueNode) 
            (Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 6, posLine = 0, posColumn = 1}}) BinaryOpNode 
            (Leaf (Token {tokenType = "float", tokenVal = Just (FloatValue 53.9), tokenPos = Position {posIndex = 12, posLine = 0, posColumn = 1}}) ValueNode
         ))createInterpreter)))
      @?= "131.9"
    , testCase "Minus operation" $
        show (getValueSafe (currentResult (visit(
          Tree 
            (Leaf (Token {tokenType = "float", tokenVal = Just (FloatValue 0.78), tokenPos = Position {posIndex = 5, posLine = 0, posColumn = 1}}) ValueNode) 
            (Token {tokenType = "-", tokenVal = Nothing, tokenPos = Position {posIndex = 6, posLine = 0, posColumn = 1}}) BinaryOpNode 
            (Leaf (Token {tokenType = "float", tokenVal = Just (FloatValue 1.0e-3), tokenPos = Position {posIndex = 13, posLine = 0, posColumn = 1}}) ValueNode)
        )createInterpreter)))
      @?= "0.779"
    , testCase "Division operation" $
      show (getValueSafe (currentResult (visit(
        Tree 
          (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 15), tokenPos = Position {posIndex = 3, posLine = 0, posColumn = 1}}) ValueNode) 
          (Token {tokenType = "/", tokenVal = Nothing, tokenPos = Position {posIndex = 4, posLine = 0, posColumn = 1}}) BinaryOpNode 
          (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 75), tokenPos = Position {posIndex = 8, posLine = 0, posColumn = 1}}) ValueNode)
      )createInterpreter)))
          @?= "0"
    , testCase "Parenthesizes" $
      show (getValueSafe (currentResult (visit(
        Tree 
          (Tree 
            (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 5), tokenPos = Position {posIndex = 3, posLine = 0, posColumn = 1}}) ValueNode) 
            (Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 4, posLine = 0, posColumn = 1}}) BinaryOpNode 
            (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 10), tokenPos = Position {posIndex = 8, posLine = 0, posColumn = 1}}) ValueNode)) 
          (Token {tokenType = "/", tokenVal = Nothing, tokenPos = Position {posIndex = 10, posLine = 0, posColumn = 1}}) BinaryOpNode 
          (Tree 
            (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 4), tokenPos = Position {posIndex = 14, posLine = 0, posColumn = 1}}) ValueNode) 
            (Token {tokenType = "-", tokenVal = Nothing, tokenPos = Position {posIndex = 15, posLine = 0, posColumn = 1}}) BinaryOpNode 
            (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 18, posLine = 0, posColumn = 1}}) ValueNode)) )createInterpreter)))
        @?= "5"
    , testCase "If statement" $
      show (getValueSafe (currentResult (visit(
        Tree 
          (Tree 
            (Tree 
              (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 10, posLine = 0, posColumn = 1}}) ValueNode) 
              (Token {tokenType = "==", tokenVal = Nothing, tokenPos = Position {posIndex = 11, posLine = 0, posColumn = 1}}) BinaryOpNode 
              (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 15, posLine = 0, posColumn = 1}}) ValueNode)) 
            (Token {tokenType = "whatif", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}) NoType 
            (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 10), tokenPos = Position {posIndex = 22, posLine = 0, posColumn = 1}}) ValueNode)) 
          (Token {tokenType = "whatif", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}) IfNode 
          Empty )createInterpreter)))
        @?= "10"
    , testCase "Until loop" $
      show (getValueSafe (currentResult (visit(
          Tree 
            (Tree 
              (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "c"), tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}) VarAccessNode) 
              (Token {tokenType = "=", tokenVal = Nothing, tokenPos = Position {posIndex = 3, posLine = 0, posColumn = 1}}) VarAssignNode 
              (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 0), tokenPos = Position {posIndex = 6, posLine = 0, posColumn = 1}}) ValueNode)) 
            (Token {tokenType = ";", tokenVal = Nothing, tokenPos = Position {posIndex = 6, posLine = 0, posColumn = 1}}) SeparatorNode 
            (Tree 
              (Tree 
                (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "c"), tokenPos = Position {posIndex = 15, posLine = 0, posColumn = 1}}) VarAccessNode) 
                (Token {tokenType = ">=", tokenVal = Nothing, tokenPos = Position {posIndex = 17, posLine = 0, posColumn = 1}}) BinaryOpNode 
                (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 30), tokenPos = Position {posIndex = 22, posLine = 0, posColumn = 1}}) ValueNode)) 
              (Token {tokenType = "until", tokenVal = Nothing, tokenPos = Position {posIndex = 8, posLine = 0, posColumn = 1}}) UntilNode 
              (Tree 
                (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "c"), tokenPos = Position {posIndex = 26, posLine = 0, posColumn = 1}}) VarAccessNode) 
                (Token {tokenType = "=", tokenVal = Nothing, tokenPos = Position {posIndex = 28, posLine = 0, posColumn = 1}}) VarAssignNode 
                (Tree 
                  (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "c"), tokenPos = Position {posIndex = 30, posLine = 0, posColumn = 1}}) VarAccessNode) 
                  (Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 32, posLine = 0, posColumn = 1}}) BinaryOpNode 
                  (Leaf (Token {tokenType = "integer", tokenVal = Just (IntValue 1), tokenPos = Position {posIndex = 35, posLine = 0, posColumn = 1}}) ValueNode
                )))))
         createInterpreter)))
             @?= "30"

    , testCase "Create function" $
      show (getValueSafe (currentResult (visit(
        Tree 
          (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "test"), tokenPos = Position {posIndex = 9, posLine = 0, posColumn = 1}}) NoType) 
          (Token {tokenType = "process", tokenVal = Nothing, tokenPos = Position {posIndex = 1, posLine = 0, posColumn = 1}}) FunctionAssignNode 
          (Tree (Branch (Token {tokenType = "identifier", tokenVal = Just (StringValue "a"), tokenPos = Position {posIndex = 15, posLine = 0, posColumn = 1}}) NoType 
            (Branch (Token {tokenType = "identifier", tokenVal = Just (StringValue "b"), tokenPos = Position {posIndex = 18, posLine = 0, posColumn = 1}}) NoType 
              (Branch (Token {tokenType = "identifier", tokenVal = Just (StringValue "c"), tokenPos = Position {posIndex = 21, posLine = 0, posColumn = 1}}) NoType 
                (Branch (Token {tokenType = "identifier", tokenVal = Just (StringValue "d"), tokenPos = Position {posIndex = 24, posLine = 0, posColumn = 1}}) NoType Empty)))) 
                (Token {tokenType = "{", tokenVal = Nothing, tokenPos = Position {posIndex = 14, posLine = 0, posColumn = 1}}) NoType 
                (Tree 
                  (Tree 
                    (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "a"), tokenPos = Position {posIndex = 30, posLine = 0, posColumn = 1}}) VarAccessNode) 
                    (Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 32, posLine = 0, posColumn = 1}}) BinaryOpNode 
                    (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "c"), tokenPos = Position {posIndex = 34, posLine = 0, posColumn = 1}}) VarAccessNode)) 
                  (Token {tokenType = "+", tokenVal = Nothing, tokenPos = Position {posIndex = 36, posLine = 0, posColumn = 1}}) BinaryOpNode 
                  (Leaf (Token {tokenType = "identifier", tokenVal = Just (StringValue "d"), tokenPos = Position {posIndex = 38, posLine = 0, posColumn = 1}}) VarAccessNode)))
         )createInterpreter)))
             @?= "null"

    ]