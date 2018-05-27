module ABNFU.Parser.LL1 where

import           ABNFU.ABNF      (Chars (CharsList, CharsRange),
                                  RuleName (RuleName))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype Token = Token Integer

data Rule = Rule !RuleName !Expr

data Expr
    = ExprRule !RuleName
    | ExprChars !Chars
    | ExprConcat !Expr !Expr
    | ExprAlternative !Expr !Expr

-- | Match path (either successful or unsuccessful).
data Path
    = PathNode !Chars !(Maybe Integer)
    | PathConcat !Path !Path
    | PathRule !RuleName !Path

type Rules = Map RuleName Rule

{-
- Feed one token at a time
- Produce list of failed parse paths
- Eventually produce successful parse path
-}

-- newtype Consumer = Consumer (Token -> ([Path], Either Consumer Path))

type Consumer = Token -> Result

data Result
    = Result [Path] State

data State
    = Continue Consumer
    | Matched Path
    | NoMatch
    | ErrMissingRule RuleName

