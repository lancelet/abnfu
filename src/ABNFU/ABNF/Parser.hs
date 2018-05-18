{-|
Module      : ABNFU.ABNF.Parser
Description : Megaparsec parser for ABNF grammars.

Parses ABNF grammars according to the rules of:
  - RFC-5234: Augmented BNF for Syntax Specifications: ABNF
  - RFC-7405: Case-Sensitive String Support in ABNF
-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module ABNFU.ABNF.Parser
    ( -- * Functions
      literalChars
    , ruleName
    ) where

import           ABNFU.ABNF.Grammar        (ABNFGrammar (ABNFGrammar),
                                            Base (Binary, Decimal, Hexadecimal),
                                            CaseSensitivity (CaseInsensitive, CaseSensitive),
                                            Chars (CharsList, CharsRange),
                                            Comment (Comment),
                                            Elem (ElemAlternative, ElemChars, ElemConcat, ElemNamedRule, ElemParen, ElemRepeat, ElemString),
                                            Line (LineBlank, LineComment, LineRule),
                                            LiteralChars (LiteralChars),
                                            LiteralString (LiteralString),
                                            Repeats (RepeatsAtLeast, RepeatsAtMost, RepeatsBetween, RepeatsExactly, RepeatsOptional),
                                            Rule (RuleBase, RuleIncremental),
                                            RuleName (RuleName))
import           Control.Monad.Combinators (sepBy1)
import qualified Data.CaseInsensitive      as CI
import           Data.Char                 (isAsciiLower, isDigit, isHexDigit, ord)
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NE
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Void                 (Void)
import           Numeric                   (readDec, readHex, readInt)
import           Text.Megaparsec           (Parsec, takeWhile1P, try, (<|>), takeWhileP)
import           Text.Megaparsec.Char      (char, char', satisfy)


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
        Decimal     -> chars decVal
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

    decVal :: Parser Integer
    decVal = takeWhile1P Nothing isDigit >>= runUniqueReadS readDec

    hexVal :: Parser Integer
    hexVal = takeWhile1P Nothing isUpperCaseHex >>= runUniqueReadS readHex

    isUpperCaseHex :: Char -> Bool
    isUpperCaseHex c = isHexDigit c && (not $ isAsciiLower c)


-- | Runs a 'ReadS' parser on a given piece of 'Text', and fails if there is
--   not a unique parse result.
runUniqueReadS :: ReadS a -> Text -> Parser a
runUniqueReadS rs txt =
    case (rs . T.unpack) txt of
        [ (x, "") ] -> pure x
        _           -> fail "unexpected parse error in runUniqueReadS"
