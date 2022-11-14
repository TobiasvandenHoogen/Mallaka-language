import Test.Tasty

import Test.UnitTests
import Test.IntegrationTests 
import Test.SystemTests


main = defaultMain allTypeTests  



-- | All tests (Unit, Integration, System)
allTypeTests :: TestTree 
allTypeTests = testGroup "Tests" [unitTests, integrationTests, systemTests]
