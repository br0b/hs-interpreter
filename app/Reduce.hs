{-# LANGUAGE LambdaCase #-}

module Reduce (reduce) where

import Data.List.NonEmpty qualified as NonEmptyList
import Data.Map (Map, empty, findWithDefault, insert, lookup)
import Syntax (Def (Def), Expr (Var, (:$)), Name, Pat, Prog (Prog))

type DefMap = Data.Map.Map Name Def

type PatMap = Data.Map.Map Pat Expr

data RstepApplicationResult
  = ReducedExpr Expr
  | PartialApplication PatMap (NonEmptyList.NonEmpty Pat) Expr

reduce :: Prog -> IO ()
reduce prog = mapM_ print (rpathFromProg prog)

rpathFromProg :: Prog -> [Expr]
rpathFromProg prog = do
  let defMap = buildDefMap prog
  case Data.Map.lookup "main" defMap of
    Just (Def _ pats expr) ->
      if null pats
        then rpath defMap expr
        else error "Main combinator has arguments!"
    Nothing -> error "Progrm has no main combinator"

rpath :: DefMap -> Expr -> [Expr]
rpath defMap expr = take 30 $ expr : maybe [] (rpath defMap) (rstep defMap expr)

rstep :: DefMap -> Expr -> Maybe Expr
rstep defMap expr = case rstepApplication defMap expr of
  Just (ReducedExpr reducedExpr) -> Just reducedExpr
  _ -> rstepDfs defMap expr

rstepApplication :: DefMap -> Expr -> Maybe RstepApplicationResult
rstepApplication defMap (Var name) =
  Data.Map.lookup name defMap
    >>= Just
      . ( \(Def _ pats defExpr) -> case NonEmptyList.nonEmpty pats of
            Just nonEmptyPats -> PartialApplication empty nonEmptyPats defExpr
            Nothing -> ReducedExpr defExpr
        )
rstepApplication defMap (x :$ y) =
  rstepApplication defMap x
    >>= Just
      . ( \case
            ReducedExpr reducedX -> (ReducedExpr (reducedX :$ y))
            PartialApplication patMap (pat NonEmptyList.:| pats) defExpr ->
              let newPatMap = insert pat y patMap
               in case NonEmptyList.nonEmpty pats of
                    Just nonEmptyPats -> PartialApplication newPatMap nonEmptyPats defExpr
                    Nothing -> ReducedExpr (subst newPatMap defExpr)
        )

-- This doesn't try to apply any combinators.
-- It just calls rstep on the expression recursively from left to right.
rstepDfs :: DefMap -> Expr -> Maybe Expr
rstepDfs _ (Var _) = Nothing
rstepDfs defMap (x :$ y) = case rstepDfs defMap x of
  Just reducedX -> Just (reducedX :$ y)
  Nothing -> rstep defMap y >>= (\reducedY -> Just (x :$ reducedY))

buildDefMap :: Prog -> DefMap
buildDefMap (Prog x) = foldr (\def@(Def name _ _) -> insert name def) empty x

subst :: PatMap -> Expr -> Expr
subst patMap var@(Var name) = findWithDefault var name patMap
subst patMap (x :$ y) = subst patMap x :$ subst patMap y