module ABNFU.Parser.LL1 where

import           ABNFU.ABNF      (Chars (CharsList, CharsRange), Repeats (RepeatsAny, RepeatsAtLeast, RepeatsAtMost, RepeatsBetween, RepeatsExactly),
                                  RuleName (RuleName))
import           Data.Foldable   (elem)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype Token = Token Integer

data Rule
    = Rule
      { ruleRuleName :: !RuleName
      , ruleExpr     :: !Expr
      }

data Expr
    = ExprRule !RuleName
    | ExprChars !Chars
    | ExprConcat !Expr !Expr
    | ExprAlternative !Expr !Expr
    | ExprRepeats Repeats !Expr

-- | Match path (either successful or unsuccessful).
data Path
    = PathNode !Chars !(Maybe Integer)
    | PathConcat !Path !(Maybe Path)
    | PathRule !RuleName !Path

type Rules = Map RuleName Rule

data Result
    = Result [Path] State

data State
    = Matched [Token] Path
    | NoMatch
    | ErrMissingRule RuleName
    | ErrTokensExhausted

feed :: Rules -> Expr -> [Token] -> Result
feed _  _ [] = Result [] ErrTokensExhausted
feed rules expr tokens@((Token t):ts) =

    case expr of

        ExprRule name ->
            case ruleExpr <$> Map.lookup name rules of
                Nothing -> Result [] (ErrMissingRule name)
                Just expr' ->
                    mapPathResult (PathRule name) result
                  where
                        result = feed rules expr' tokens

        ExprChars chars@(CharsList is) ->
            if elem t is
            then Result [] (Matched ts (PathNode chars (Just t)))
            else Result [PathNode chars Nothing] NoMatch

        ExprChars chars@(CharsRange l h) ->
            if t >= l && t <= h
            then Result [] (Matched ts (PathNode chars (Just t)))
            else Result [PathNode chars Nothing] NoMatch

        ExprConcat exprA exprB ->
            case feed rules exprA tokens of
                Result ps (Matched tokens' p) ->
                    prependPaths ps $
                    mapPathResult (PathConcat p . Just) (feed rules exprB tokens')
                failResult ->
                    mapPathResult (\x -> PathConcat x Nothing) failResult

        ExprAlternative exprA exprB ->
            case feed rules exprA tokens of
                r@(Result _ (Matched _ _)) ->
                    r
                _ ->
                    feed rules exprB tokens

        ExprRepeats (RepeatsExactly 1) expr' ->
            feed rules expr' tokens
        ExprRepeats (RepeatsExactly n) expr' ->
            feed rules e tokens
          where
            r' = ExprRepeats (RepeatsExactly (n - 1)) expr'
            e = ExprConcat expr' r'
        ExprRepeats (RepeatsAtLeast 1) expr' ->
            undefined  -- TODO: expr' is optional
        ExprRepeats (RepeatsAtLeast n) expr' ->
            feed rules e tokens
          where
            r' = ExprRepeats (RepeatsAtLeast (n - 1)) expr'
            e = ExprConcat expr' r'
        ExprRepeats (RepeatsAtMost n) expr' ->
            undefined -- TODO: expr' is optional; followed by RepeatsAtMost
        ExprRepeats (RepeatsBetween 0 h) expr' ->
            undefined -- TODO: expr' 

prependPaths :: [Path] -> Result -> Result
prependPaths ps (Result ps' x) = Result (ps ++ ps') x

mapPathResult :: (Path -> Path) -> Result -> Result
mapPathResult f (Result ps s) = Result (fmap f ps) (mapPathState f s)

mapPathState :: (Path -> Path) -> State -> State
mapPathState f s =
    case s of
        Matched ts p        -> Matched ts (f p)
        NoMatch             -> NoMatch
        ErrMissingRule name -> ErrMissingRule name
        ErrTokensExhausted  -> ErrTokensExhausted
