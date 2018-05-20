{-|
Module      : ABNFU.ABNF.Parser
Description : Megaparsec parser for ABNF grammars.

Parses ABNF grammars according to the rules of:
  - RFC-5234: Augmented BNF for Syntax Specifications: ABNF
  - RFC-7405: Case-Sensitive String Support in ABNF
-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module ABNFU.ABNF.Parser
    ( -- * Functions
      abnf
    , block
    , rule
    , ruleName
    , pElem
    , literalChars
    , literalString
    , repeats
    , comment
    , cwsp
    , cnl
    ) where

import           ABNFU.ABNF.Grammar        (ABNFGrammar (ABNFGrammar),
                                            Base (Binary, Decimal, Hexadecimal),
                                            Block (BlockLineComment, BlockRule),
                                            CaseSensitivity (CaseInsensitive, CaseSensitive),
                                            Chars (CharsList, CharsRange),
                                            Comment (Comment),
                                            Elem (ElemAlternative, ElemChars, ElemConcat, ElemNamedRule, ElemOptional, ElemParen, ElemRepeat, ElemString),
                                            LiteralChars (LiteralChars),
                                            LiteralString (LiteralString),
                                            Repeats (RepeatsAny, RepeatsAtLeast, RepeatsAtMost, RepeatsBetween, RepeatsExactly),
                                            Rule (RuleBase, RuleIncremental),
                                            RuleName (RuleName))
import           Control.Monad.Combinators (many, optional, sepBy1, some)
import qualified Data.CaseInsensitive      as CI
import           Data.Char                 (isAsciiLower, isDigit, isHexDigit,
                                            ord)
import           Data.Either               (rights)
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NE
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Void                 (Void)
import           Numeric                   (readDec, readHex, readInt)
import           Text.Megaparsec           (Parsec, takeWhile1P, takeWhileP,
                                            try, (<|>))
import           Text.Megaparsec.Char      (char, char', satisfy, string,
                                            string')


type Parser = Parsec Void Text


abnf :: Parser [Block]
abnf = rights <$> many block


block :: Parser (Either () Block)
block =
        (Left <$> try (many wsp *> nlChars))
    <|> ((Right . BlockLineComment) <$> try (many wsp *> comment))
    <|> ((Right . BlockRule) <$> rule)


rule :: Parser Rule
rule = try ruleBase <|> try ruleIncremental

  where

    ruleBase :: Parser Rule
    ruleBase = RuleBase <$> (ruleName <* many cwsp <* char '=' <* many cwsp) <*> pElem <* cnl

    ruleIncremental :: Parser Rule
    ruleIncremental = RuleIncremental <$> (ruleName <* many cwsp <* string "/=" <* many cwsp) <*> pElem <* cnl



ruleName :: Parser RuleName
ruleName =
    (RuleName . CI.mk)
    <$> (T.cons <$> satisfy isAsciiAlpha <*> takeWhileP Nothing isNameChar)

  where

    isAsciiAlpha :: Char -> Bool
    isAsciiAlpha c =
        let
            c' = ord c
        in
               (c' >= 65 && c' <= 90)
            || (c' >= 97 && c' <= 122)

    isNameChar :: Char -> Bool
    isNameChar c = isAsciiAlpha c || isDigit c || c == '-'


pElem :: Parser Elem
pElem = alternation <* many wsp  {- note: many wsp is from the erratum -}

  where

    alternation :: Parser Elem
    alternation =
            try (ElemAlternative <$> (concatenation <* many cwsp <* char '/' <* many cwsp) <*> alternation)
        <|> concatenation

    concatenation :: Parser Elem
    concatenation =
            try (ElemConcat <$> (repetition <* some cwsp) <*> concatenation)
        <|> repetition

    repetition :: Parser Elem
    repetition =
            try (ElemRepeat <$> repeats <*> element)
        <|> element

    element :: Parser Elem
    element =
            terminal
        <|> group
        <|> option

    group :: Parser Elem
    group = bracketed '(' ')' (ElemParen <$> alternation)

    option :: Parser Elem
    option = bracketed '[' ']' (ElemOptional <$> alternation)

    terminal :: Parser Elem
    terminal =
            (ElemChars <$> literalChars)
        <|> (ElemString <$> literalString)
        <|> (ElemNamedRule <$> ruleName)

    bracketed :: Char -> Char -> Parser a -> Parser a
    bracketed c1 c2 p =
        char c1 *> many cwsp *> p <* many cwsp <* char c2


-- | Whitespace, or a newline and then more whitespace.
cwsp :: Parser ()
cwsp =
    (wsp <|> (cnl *> wsp))
    *> pure ()


-- | Comment or one or more characters of a newline.
cnl :: Parser ()
cnl = (comment *> pure ()) <|> nlChars


-- | One or more characters of a newline.
nlChars :: Parser ()
nlChars = takeWhile1P Nothing isNL *> pure ()
  where
    isNL c = (c == '\n') || (c == '\r')


-- | Single character of whitespace.
wsp :: Parser ()
wsp = (char ' ' <|> char '\t') *> pure ()


literalChars :: Parser LiteralChars
literalChars = do
    _ <- char '%'
    b <- base
    cs <- case b of
        Binary      -> chars binVal
        Decimal     -> chars integer
        Hexadecimal -> chars hexVal
    pure (LiteralChars b cs)

  where

    base :: Parser Base
    base =
            (char' 'b' *> pure Binary)
        <|> (char' 'd' *> pure Decimal)
        <|> (char' 'x' *> pure Hexadecimal)

    chars :: Parser Integer -> Parser Chars
    chars p =
            try (CharsRange <$> (p <* char '-') <*> p)
        <|> ((CharsList . NE.fromList) <$> p `sepBy1` char '.')

    binVal :: Parser Integer
    binVal = do
        let isBinDigit c = c == '0' || c == '1'
        let toInt c = case c of
                '0' -> 0
                '1' -> 1
                _   -> error "unexpected binary character"
        txt <- takeWhile1P Nothing isBinDigit
        runUniqueReadS (readInt 2 (const True) toInt) txt

    hexVal :: Parser Integer
    hexVal = takeWhile1P Nothing isUpperCaseHex >>= runUniqueReadS readHex

    isUpperCaseHex :: Char -> Bool
    isUpperCaseHex c = isHexDigit c && (not $ isAsciiLower c)


literalString :: Parser LiteralString
literalString = LiteralString <$> caseSensitivity <*> quotedString

  where

    caseSensitivity :: Parser (Maybe CaseSensitivity)
    caseSensitivity =
        optional
        (    (string' "%i" *> pure CaseInsensitive)
         <|> (string' "%s" *> pure CaseSensitive)
        )

    quotedString :: Parser Text
    quotedString = char '"' *> takeWhileP Nothing isStringContent <* char '"'

    isStringContent :: Char -> Bool
    isStringContent c =
        let
            c' = ord c
        in
            (c' == 32) || (c' == 33) || (c' >= 35 && c' <= 126)


repeats :: Parser Repeats
repeats =
        try (RepeatsBetween <$> (integer <* char '*') <*> integer)
    <|> try (RepeatsAtLeast <$> (integer <* char '*'))
    <|> try (RepeatsAtMost  <$> (char '*' *> integer))
    <|> try (RepeatsExactly <$> integer)
    <|> try (char '*' *> pure RepeatsAny)


comment :: Parser Comment
comment = char ';' *> (Comment <$> takeWhileP Nothing isVCharOrWS) <* nlChars
  where
    isVCharOrWS :: Char -> Bool
    isVCharOrWS c =
        let
            c' = ord c
        in
            (c == ' ') || (c == '\t') || (c' >= 33 && c' <= 126)


-- | Parses a decimal integer.
integer :: Parser Integer
integer = takeWhile1P Nothing isDigit >>= runUniqueReadS readDec


-- | Runs a 'ReadS' parser on a given piece of 'Text', and fails if there is
--   not a unique parse result.
runUniqueReadS :: ReadS a -> Text -> Parser a
runUniqueReadS rs txt =
    case (rs . T.unpack) txt of
        [ (x, "") ] -> pure x
        _           -> fail "unexpected parse error in runUniqueReadS"
