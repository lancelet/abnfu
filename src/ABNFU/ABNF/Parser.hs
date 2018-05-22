{-|
Module      : ABNFU.ABNF.Parser
Description : Megaparsec parser for ABNF grammars.

Parses ABNF grammars according to the rules of:
  - RFC-5234: Augmented BNF for Syntax Specifications: ABNF
  - RFC-7405: Case-Sensitive String Support in ABNF

All parts of ABNF except free-form prose are understood.
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

import qualified ABNFU.ABNF.Grammar        as G

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


-- | Parses a sequence of blocks forming an ABNF grammar.
abnf :: Parser [G.Block]
abnf = rights <$> many block


block :: Parser (Either () G.Block)
block =
        (Left <$> try (many wsp *> nlChars))
    <|> ((Right . G.BlockLineComment) <$> try (many wsp *> comment))
    <|> ((Right . G.BlockRule) <$> rule)


rule :: Parser G.Rule
rule = try ruleBase <|> try ruleIncremental

  where

    ruleBase :: Parser G.Rule
    ruleBase
        =   G.RuleBase
        <$> (ruleName <* many cwsp <* char '=' <* many cwsp)
        <*> pElem <* cnl

    ruleIncremental :: Parser G.Rule
    ruleIncremental
        =   G.RuleIncremental
        <$> (ruleName <* many cwsp <* string "/=" <* many cwsp)
        <*> pElem <* cnl



ruleName :: Parser G.RuleName
ruleName =
    (G.RuleName . CI.mk)
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


pElem :: Parser G.Elem
pElem = alternation <* many wsp  {- note: many wsp is from the erratum -}

  where

    alternation :: Parser G.Elem
    alternation =
            try (    G.ElemAlternative
                 <$> (concatenation <* many cwsp <* char '/' <* many cwsp)
                 <*> alternation
                )
        <|> concatenation

    concatenation :: Parser G.Elem
    concatenation =
            try (G.ElemConcat <$> (repetition <* some cwsp) <*> concatenation)
        <|> repetition

    repetition :: Parser G.Elem
    repetition =
            try (G.ElemRepeat <$> repeats <*> element)
        <|> element

    element :: Parser G.Elem
    element =
            terminal
        <|> group
        <|> option

    group :: Parser G.Elem
    group = bracketed '(' ')' (G.ElemParen <$> alternation)

    option :: Parser G.Elem
    option = bracketed '[' ']' (G.ElemOptional <$> alternation)

    terminal :: Parser G.Elem
    terminal =
            (G.ElemChars <$> literalChars)
        <|> (G.ElemString <$> literalString)
        <|> (G.ElemNamedRule <$> ruleName)

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


literalChars :: Parser G.LiteralChars
literalChars = do
    _ <- char '%'
    b <- base
    cs <- case b of
        G.Binary      -> chars binVal
        G.Decimal     -> chars integer
        G.Hexadecimal -> chars hexVal
    pure (G.LiteralChars b cs)

  where

    base :: Parser G.Base
    base =
            (char' 'b' *> pure G.Binary)
        <|> (char' 'd' *> pure G.Decimal)
        <|> (char' 'x' *> pure G.Hexadecimal)

    chars :: Parser Integer -> Parser G.Chars
    chars p =
            try (G.CharsRange <$> (p <* char '-') <*> p)
        <|> ((G.CharsList . NE.fromList) <$> p `sepBy1` char '.')

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


literalString :: Parser G.LiteralString
literalString = G.LiteralString <$> caseSensitivity <*> quotedString

  where

    caseSensitivity :: Parser (Maybe G.CaseSensitivity)
    caseSensitivity =
        optional
        (    (string' "%i" *> pure G.CaseInsensitive)
         <|> (string' "%s" *> pure G.CaseSensitive)
        )

    quotedString :: Parser Text
    quotedString = char '"' *> takeWhileP Nothing isStringContent <* char '"'

    isStringContent :: Char -> Bool
    isStringContent c =
        let
            c' = ord c
        in
            (c' == 32) || (c' == 33) || (c' >= 35 && c' <= 126)


repeats :: Parser G.Repeats
repeats =
        try (G.RepeatsBetween <$> (integer <* char '*') <*> integer)
    <|> try (G.RepeatsAtLeast <$> (integer <* char '*'))
    <|> try (G.RepeatsAtMost  <$> (char '*' *> integer))
    <|> try (G.RepeatsExactly <$> integer)
    <|> try (char '*' *> pure G.RepeatsAny)


comment :: Parser G.Comment
comment = char ';' *> (G.Comment <$> takeWhileP Nothing isVCharOrWS) <* nlChars
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
