module Test.ABNFU.ABNF.GrammarTest where

import Hedgehog (Property, property, withTests)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty (testGroup)

tests :: TestTree
tests = testGroup "ABNFU.ABNF.Grammar"
    [ testProperty "unit: literalChars" unit_literalChars
    ]

unit_literalChars :: Property
unit_literalChars = withTests 1 $ property $ do
    pure ()
