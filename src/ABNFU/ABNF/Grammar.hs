{-|
Module      : ABNFU.ABNF.Grammar
Description : Haskell data types to describe an ABNF grammar.

The types in this module describe the components of an ABNF grammar
"as written", corresponding to RFC-5234 and RFC-7405.
-}
module ABNFU.ABNF.Grammar
    ( -- * Types
      ABNFGrammar(..)
    , Rule(..)
    , Elem(..)
    , RuleName(..)
    , LiteralChars(..)
    , Chars(..)
    , Base(..)
    , LiteralString(..)
    , CaseSensitivity(..)
    , Repeats(..)
    ) where

import           Data.CaseInsensitive (CI)
import           Data.List.NonEmpty   (NonEmpty)
import           Data.Text            (Text)

-- | Parsed contents of an ABNF grammar.
newtype ABNFGrammar = ABNFGrammar [Rule]

-- | ABNF rule.
data Rule
    -- | Base rule (RFC-5234 2.2).
    --
    --   > name = elements crlf
    = RuleBase !RuleName !Elem
    -- | Incremental alternatives for a rule (RFC-5234 3.3).
    --
    --   > oldrule =/ additional-alternatives
    | RuleIncremental !RuleName !Elem

-- | Expression language of rule elements.
data Elem
    -- | Uses a named rule.
    --
    --   > name1 = name2
    = ElemNamedRule !RuleName
    -- | A character literal (RFC-5234 2.3).
    --
    --   > %x61     ; 'a' (single character)
    --   > %d13.10  ; multiple characters, separated by periods
    | ElemChars !LiteralChars
    -- | A string literal (RFC-5234 2.3 and RFC-7405).
    --
    --   > "command string"    ; case-insensitive string (RFC-5234)
    --   > %i"command string"  ; case-insensitive string (RFC-7405)
    --   > %s"command string"  ; case-sensitive string (RFC-7405)
    | ElemString !LiteralString
    -- | Concatenation of elements (RFC-5234 3.1).
    --
    --   > rule = a b c
    | ElemConcat !Elem !Elem
    -- | Alternative elements (RFC-5234 3.2).
    --
    --   > rule = a / b
    | ElemAlternative !Elem !Elem
    -- | Repeated element (RFC-5234 3.6, 3.7 and 3.8).
    --
    --   > 5e     ; repeats element e 5 times
    --   > 2*5e   ; repeats e a minimum of 2 and maximum of 5 times
    --   > *5e    ; repeats e from zero to 5 times
    --   > [e]    ; e is optional
    | ElemRepeat Repeats !Elem
    -- | Parenthesized elements (RFC-5234 3.5).
    --
    --   > (a b)
    | ElemParen !Elem

-- | Name of a rule (RFC-5234 2.1).
newtype RuleName
    = RuleName (CI Text)
    deriving (Eq, Show)

-- | Numeric representation of terminal character(s) (RFC-5234 2.3, 3.4).
data LiteralChars
    = LiteralChars !Base !Chars
    deriving (Eq, Show)

-- | Specification of characters.
data Chars
    -- | Period-separated list of single character (RFC-5234 2.3).
    --
    --   These are concatenated characters.
    = CharsList !(NonEmpty Integer)
    -- | Inclusive range of characters (RFC-5234 3.4).
    | CharsRange !Integer !Integer
    deriving (Eq, Show)

-- | Base in which a numeric character is expressed (RFC-5234 2.3).
data Base
    = Binary
    | Decimal
    | Hexadecimal
    deriving (Eq, Show)

-- | Literal string (RFC-5234 2.3).
data LiteralString
    = LiteralString !(Maybe CaseSensitivity) !Text
    deriving (Eq, Show)

-- | Case sensitivity of a literal string (RFC-7405).
data CaseSensitivity
    = CaseSensitive
    | CaseInsensitive
    deriving (Eq, Show)

-- | Specifies how repeats of an element occur (RFC-5234 3.6, 3.7 and 3.8).
--
--   This covers both @<a>*<b>element@ notation and optional @[a]@ notation.
data Repeats
    -- | Specifies exactly how many times an element should repeat.
    --
    --   > 5e
    = RepeatsExactly !Integer
    -- | Specifies the minimum number of times an element should repeat.
    --
    --   > 3*e
    | RepeatsAtLeast !Integer
    -- | Specifies the maximum number of times an element should repeat.
    --
    --   > *5e
    | RepeatsAtMost !Integer
    -- | Specifies a range of times an element should repeat.
    --
    --   > 3*5e
    | RepeatsBetween !Integer !Integer
    -- | Specifies that an element is optional.
    --
    --   > [e]
    | RepeatsOptional
