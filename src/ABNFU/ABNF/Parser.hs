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
      ruleName
    , literalChars
    , literalString
    , repeats
    ) where

import           ABNFU.ABNF.Grammar        (ABNFGrammar (ABNFGrammar),
                                            Base (Binary, Decimal, Hexadecimal),
                                            CaseSensitivity (CaseInsensitive, CaseSensitive),
                                            Chars (CharsList, CharsRange),
                                            Elem (ElemAlternative, ElemChars, ElemConcat, ElemNamedRule, ElemOptional, ElemParen, ElemRepeat, ElemString),
                                            LiteralChars (LiteralChars),
                                            LiteralString (LiteralString),
                                            Repeats (RepeatsAtLeast, RepeatsAtMost, RepeatsBetween, RepeatsExactly),
                                            Rule (RuleBase, RuleIncremental),
                                            RuleName (RuleName))
import           Control.Monad.Combinators (optional, sepBy1)
import qualified Data.CaseInsensitive      as CI
import           Data.Char                 (isAsciiLower, isDigit, isHexDigit,
                                            ord)
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NE
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Void                 (Void)
import           Numeric                   (readDec, readHex, readInt)
import           Text.Megaparsec           (Parsec, takeWhile1P, takeWhileP,
                                            try, (<|>))
import           Text.Megaparsec.Char      (char, char', satisfy, string')


type Parser = Parsec Void Text


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
