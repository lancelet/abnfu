import           Test.Tasty (TestTree)
import qualified Test.Tasty as T

import qualified Test.ABNFU.ABNF.ParserTest (tests)

main :: IO ()
main = T.defaultMain tests

tests :: TestTree
tests = T.testGroup "All tests"
    [ Test.ABNFU.ABNF.ParserTest.tests
    ]
