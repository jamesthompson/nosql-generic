import           Data.Gogol.DatastoreEntityTests
import           Test.Tasty                      (TestTree, defaultMain,
                                                  testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Root"
                  [ datastoreEntityTests
                  ]

