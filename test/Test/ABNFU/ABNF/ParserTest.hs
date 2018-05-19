{-# LANGUAGE OverloadedStrings #-}
module Test.ABNFU.ABNF.ParserTest where

import           ABNFU.ABNF.Grammar  (Base (Decimal, Hexadecimal), CaseSensitivity (CaseInsensitive, CaseSensitive),
                                      Chars (CharsList, CharsRange),
                                      Elem (ElemAlternative, ElemChars, ElemConcat, ElemNamedRule, ElemOptional, ElemParen, ElemRepeat, ElemString),
                                      LiteralChars (LiteralChars),
                                      LiteralString (LiteralString),
                                      Repeats (RepeatsAtLeast, RepeatsAtMost, RepeatsBetween, RepeatsExactly),
                                      RuleName (RuleName))
import           ABNFU.ABNF.Parser   (literalChars, literalString, repeats,
                                      ruleName, pElem, comment)

import qualified Data.List.NonEmpty  as NE

import           Hedgehog            (Property, property, withTests, (===))
import           Test.Tasty          (TestTree)
import           Test.Tasty          (testGroup)
import           Test.Tasty.Hedgehog (testProperty)
import           Text.Megaparsec     (parseMaybe)


tests :: TestTree
tests = testGroup "ABNFU.ABNF.Grammar"
    [ testProperty "unit: literalChars"  unit_literalChars
    , testProperty "unit: ruleName"      unit_ruleName
    , testProperty "unit: pElem"         unit_pElem
    , testProperty "unit: literalString" unit_literalString
    , testProperty "unit: repeats"       unit_repeats
    , testProperty "unit: comment"       unit_comment
    ]


unit_ruleName :: Property
unit_ruleName = withTests 1 $ property $ do
    parseMaybe ruleName "foo-bar42" === Just (RuleName "foo-bar42")


unit_pElem :: Property
unit_pElem = withTests 1 $ property $ do

    let e1 = ElemNamedRule (RuleName "rule-name")
    parseMaybe pElem "rule-name" === Just e1

    let c1 = LiteralChars Hexadecimal (CharsRange 48 57)
    let e2 = ElemChars c1
    parseMaybe pElem "%x30-39  \n " === Just e2

    let e3 = ElemString (LiteralString Nothing "hello")
    parseMaybe pElem "\"hello\" " === Just e3

    let e4 = ElemRepeat (RepeatsBetween 3 5) e1
    parseMaybe pElem "3*5rule-name  \n  \n " === Just e4

    let e5 = ElemOptional e3
    parseMaybe pElem "[ \"hello\" ] " === Just e5

    let e6 = ElemConcat e5 e1
    parseMaybe pElem "[\"hello\"] \n rule-name" === Just e6

    let e7 = ElemAlternative e6 e4
    parseMaybe pElem "[\"hello\"] rule-name / 3*5rule-name" === Just e7

    let e8 = ElemAlternative (ElemParen e6) e4
    parseMaybe pElem "( [\"hello\"] rule-name ) / 3*5rule-name" === Just e8


unit_literalChars :: Property
unit_literalChars = withTests 1 $ property $ do

    -- RFC-5234 2.3
    let d13 = LiteralChars Decimal (CharsList (NE.fromList [13]))
    let x0d = LiteralChars Hexadecimal (CharsList (NE.fromList [13]))
    let dcrlf = LiteralChars Decimal (CharsList (NE.fromList [13, 10]))
    let d97 = LiteralChars Decimal (CharsList (NE.fromList [97]))
    let d98 = LiteralChars Decimal (CharsList (NE.fromList [98]))
    let d99 = LiteralChars Decimal (CharsList (NE.fromList [99]))
    let d9799 = LiteralChars Decimal (CharsList (NE.fromList [97, 98, 99]))
    parseMaybe literalChars "%d13" === Just d13
    parseMaybe literalChars "%x0D" === Just x0d
    parseMaybe literalChars "%d13.10" === Just dcrlf
    parseMaybe literalChars "%d97" === Just d97
    parseMaybe literalChars "%d98" === Just d98
    parseMaybe literalChars "%d99" === Just d99
    parseMaybe literalChars "%d97.98.99" === Just d9799

    -- RFC-5234 3.1
    let foo = LiteralChars Hexadecimal (CharsList (NE.fromList [97]))
    let bar = LiteralChars Hexadecimal (CharsList (NE.fromList [98]))
    parseMaybe literalChars "%x61" === Just foo
    parseMaybe literalChars "%x62" === Just bar

    -- RFC-5234 3.4
    let digit = LiteralChars Hexadecimal (CharsRange 48 57)
    parseMaybe literalChars "%x30-39" === Just digit

    -- RFC-5234 3.4
    let x0d0a = LiteralChars Hexadecimal (CharsList (NE.fromList [13, 10]))
    let x207e = LiteralChars Hexadecimal (CharsRange 32 126)
    parseMaybe literalChars "%x0D.0A" === Just x0d0a
    parseMaybe literalChars "%x20-7E" === Just x207e


unit_literalString :: Property
unit_literalString = withTests 1 $ property $ do

    let hello = LiteralString Nothing "hello"
    parseMaybe literalString "\"hello\"" === Just hello

    let empty = LiteralString Nothing ""
    parseMaybe literalString "\"\"" === Just empty

    let ciHello = LiteralString (Just CaseInsensitive) "hello"
    parseMaybe literalString "%i\"hello\"" === Just ciHello

    let csHello = LiteralString (Just CaseSensitive) "hello"
    parseMaybe literalString "%s\"hello\"" === Just csHello


unit_repeats :: Property
unit_repeats = withTests 1 $ property $ do
    parseMaybe repeats "5" === Just (RepeatsExactly 5)
    parseMaybe repeats "3*" === Just (RepeatsAtLeast 3)
    parseMaybe repeats "*5" === Just (RepeatsAtMost 5)
    parseMaybe repeats "3*5" === Just (RepeatsBetween 3 5)


unit_comment :: Property
unit_comment = withTests 1 $ property $ do
    parseMaybe comment ";\n" === Just ""
    parseMaybe comment "; foo bar\t  \n" === Just " foo bar\t  "
