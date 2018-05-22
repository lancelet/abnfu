{-|
Module      : ABNFU.ABNF
Description : ABNF grammar and parser.

Data types for the components of an ABNF grammar ("as written"), and a parser
for ABNF text files, corresponding to RFC-5234 and RFC-7405. All parts of ABNF
are supported except free-form prose.
-}
module ABNFU.ABNF
    ( -- * ABNF AST types
      G.ABNFGrammar(..)
    , G.Block(..)
    , G.Comment(..)
    , G.Rule(..)
    , G.Elem(..)
    , G.RuleName(..)
    , G.LiteralChars(..)
    , G.Chars(..)
    , G.Base(..)
    , G.LiteralString(..)
    , G.CaseSensitivity(..)
    , G.Repeats(..)
      -- * Parser for ABNF
    , ABNFParser
    , parseABNF
    ) where

import qualified ABNFU.ABNF.Grammar as G
import qualified ABNFU.ABNF.Parser as P

-- | Parser type for ABNF.
type ABNFParser = P.Parser

-- | Parses an ABNF file producing a sequence of blocks.
parseABNF :: P.Parser [G.Block]
parseABNF = P.abnf
