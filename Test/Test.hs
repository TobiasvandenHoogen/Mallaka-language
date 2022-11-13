import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Source.Lexer
import Source.Parser
import Source.Interpreter
import Source.Exception

import Source.Types

import Test.UnitTests
import Test.IntegrationTests 
import Test.SystemTests

main = defaultMain allTypeTests  



-- | All tests (Unit, Integration, System)
allTypeTests :: TestTree 
allTypeTests = testGroup "Tests" [unitTests, integrationTests, systemTests]
