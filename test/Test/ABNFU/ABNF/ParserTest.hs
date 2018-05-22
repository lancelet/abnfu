{-# LANGUAGE OverloadedStrings #-}
module Test.ABNFU.ABNF.ParserTest where

import qualified ABNFU.ABNF.Grammar     as G
import           ABNFU.ABNF.Parser      (abnf, block, comment, literalChars,
                                         literalString, pElem, repeats, rule,
                                         ruleName)

import           Control.Monad.IO.Class (liftIO)
import qualified Data.List.NonEmpty     as NE
import qualified Data.Text              as T
import qualified Data.Text.IO           as T

import           Hedgehog               (Property, property, withTests, (===))
import           Test.Tasty             (TestTree)
import           Test.Tasty             (testGroup)
import           Test.Tasty.Hedgehog    (testProperty)
import           Text.Megaparsec        (parseMaybe)


tests :: TestTree
tests = testGroup "ABNFU.ABNF.Grammar"
    [ testProperty "unit (IO): abnf core"    unit_abnfcore
    , testProperty "unit (IO): abnf in abnf" unit_abnfabnf
    , testProperty "unit: block"             unit_block
    , testProperty "unit: rule"              unit_rule
    , testProperty "unit: literalChars"      unit_literalChars
    , testProperty "unit: ruleName"          unit_ruleName
    , testProperty "unit: pElem"             unit_pElem
    , testProperty "unit: literalString"     unit_literalString
    , testProperty "unit: repeats"           unit_repeats
    , testProperty "unit: comment"           unit_comment
    ]


unit_abnfcore :: Property
unit_abnfcore = withTests 1 $ property $ do
    core <- liftIO $ T.readFile "doc/core.abnf"
    case parseMaybe abnf core of
        Just _  -> pure ()
        Nothing -> fail "Could not parse core ABNF: doc/core.abnf"


unit_abnfabnf :: Property
unit_abnfabnf = withTests 1 $ property $ do
    gabnf <- liftIO $ T.readFile "doc/abnf.abnf"
    case parseMaybe abnf gabnf of
        Just _  -> pure ()
        Nothing -> fail "Could not parse ABNF definition: doc/abnf.abnf"


unit_block :: Property
unit_block = withTests 1 $ property $ do

    let e1 = G.ElemString (G.LiteralString Nothing "hello")
    let e2 = G.ElemOptional e1
    let e3 = G.ElemChars (G.LiteralChars G.Hexadecimal (G.CharsRange 48 57))
    let e4 = G.ElemConcat e2 e3
    let r1 = G.RuleBase (G.RuleName "rule-name") e4

    let b1 = G.BlockRule r1
    let i1 = T.unlines
             [ "rule-name ="
             , "  [\"hello\"]  ; literal string hello"
             , "  %x30-39      ; character range"
             , ""
             ]
    parseMaybe block i1 === Just (Right b1)

    let b2 = G.BlockLineComment (G.Comment " this is a comment")
    parseMaybe block "  ; this is a comment\n" === Just (Right b2)

    parseMaybe block "  \n" === Just (Left ())


unit_rule :: Property
unit_rule = withTests 1 $ property $ do

    let e1 = G.ElemString (G.LiteralString Nothing "hello")
    let e2 = G.ElemOptional e1
    let e3 = G.ElemChars (G.LiteralChars G.Hexadecimal (G.CharsRange 48 57))
    let e4 = G.ElemConcat e2 e3

    let r1 = G.RuleBase (G.RuleName "rule-name") e4
    parseMaybe rule "rule-name   =   [\"hello\"] %x30-39 \n" === Just r1

    let r2 = G.RuleIncremental (G.RuleName "rule-name") e4
    parseMaybe rule "rule-name /= [\"hello\"] %x30-39 ; comment\n" === Just r2


unit_ruleName :: Property
unit_ruleName = withTests 1 $ property $ do
    parseMaybe ruleName "foo-bar42" === Just (G.RuleName "foo-bar42")


unit_pElem :: Property
unit_pElem = withTests 1 $ property $ do

    let e1 = G.ElemNamedRule (G.RuleName "rule-name")
    parseMaybe pElem "rule-name" === Just e1

    let c1 = G.LiteralChars G.Hexadecimal (G.CharsRange 48 57)
    let e2 = G.ElemChars c1
    parseMaybe pElem "%x30-39  " === Just e2

    let e3 = G.ElemString (G.LiteralString Nothing "hello")
    parseMaybe pElem "\"hello\" " === Just e3

    let e4 = G.ElemRepeat (G.RepeatsBetween 3 5) e1
    parseMaybe pElem "3*5rule-name " === Just e4

    let e5 = G.ElemOptional e3
    parseMaybe pElem "[ \"hello\" ] " === Just e5

    let e6 = G.ElemConcat e5 e1
    parseMaybe pElem "[\"hello\"] \n rule-name" === Just e6
    parseMaybe pElem "[\"hello\"]  ; comment \n rule-name" === Just e6

    let e7 = G.ElemAlternative e6 e4
    parseMaybe pElem "[\"hello\"] rule-name / 3*5rule-name" === Just e7

    let e8 = G.ElemAlternative (G.ElemParen e6) e4
    parseMaybe pElem "( [\"hello\"] rule-name ) / 3*5rule-name" === Just e8

    let e9 = G.ElemConcat e3 e2
    parseMaybe pElem "\"hello\"\n  %x30-39" === Just e9


unit_literalChars :: Property
unit_literalChars = withTests 1 $ property $ do

    -- RFC-5234 2.3
    let d13 = G.LiteralChars G.Decimal (G.CharsList (NE.fromList [13]))
    let x0d = G.LiteralChars G.Hexadecimal (G.CharsList (NE.fromList [13]))
    let dcrlf = G.LiteralChars G.Decimal (G.CharsList (NE.fromList [13, 10]))
    let d97 = G.LiteralChars G.Decimal (G.CharsList (NE.fromList [97]))
    let d98 = G.LiteralChars G.Decimal (G.CharsList (NE.fromList [98]))
    let d99 = G.LiteralChars G.Decimal (G.CharsList (NE.fromList [99]))
    let d9799 = G.LiteralChars G.Decimal (G.CharsList (NE.fromList [97, 98, 99]))
    parseMaybe literalChars "%d13" === Just d13
    parseMaybe literalChars "%x0D" === Just x0d
    parseMaybe literalChars "%d13.10" === Just dcrlf
    parseMaybe literalChars "%d97" === Just d97
    parseMaybe literalChars "%d98" === Just d98
    parseMaybe literalChars "%d99" === Just d99
    parseMaybe literalChars "%d97.98.99" === Just d9799

    -- RFC-5234 3.1
    let foo = G.LiteralChars G.Hexadecimal (G.CharsList (NE.fromList [97]))
    let bar = G.LiteralChars G.Hexadecimal (G.CharsList (NE.fromList [98]))
    parseMaybe literalChars "%x61" === Just foo
    parseMaybe literalChars "%x62" === Just bar

    -- RFC-5234 3.4
    let digit = G.LiteralChars G.Hexadecimal (G.CharsRange 48 57)
    parseMaybe literalChars "%x30-39" === Just digit

    -- RFC-5234 3.4
    let x0d0a = G.LiteralChars G.Hexadecimal (G.CharsList (NE.fromList [13, 10]))
    let x207e = G.LiteralChars G.Hexadecimal (G.CharsRange 32 126)
    parseMaybe literalChars "%x0D.0A" === Just x0d0a
    parseMaybe literalChars "%x20-7E" === Just x207e


unit_literalString :: Property
unit_literalString = withTests 1 $ property $ do

    let hello = G.LiteralString Nothing "hello"
    parseMaybe literalString "\"hello\"" === Just hello

    let empty = G.LiteralString Nothing ""
    parseMaybe literalString "\"\"" === Just empty

    let ciHello = G.LiteralString (Just G.CaseInsensitive) "hello"
    parseMaybe literalString "%i\"hello\"" === Just ciHello

    let csHello = G.LiteralString (Just G.CaseSensitive) "hello"
    parseMaybe literalString "%s\"hello\"" === Just csHello


unit_repeats :: Property
unit_repeats = withTests 1 $ property $ do
    parseMaybe repeats "5"   === Just (G.RepeatsExactly 5)
    parseMaybe repeats "3*"  === Just (G.RepeatsAtLeast 3)
    parseMaybe repeats "*5"  === Just (G.RepeatsAtMost 5)
    parseMaybe repeats "3*5" === Just (G.RepeatsBetween 3 5)
    parseMaybe repeats "*"   === Just (G.RepeatsAny)


unit_comment :: Property
unit_comment = withTests 1 $ property $ do
    parseMaybe comment ";\n" === Just (G.Comment "")
    parseMaybe comment "; foo bar\t  \n" === Just (G.Comment " foo bar\t  ")
