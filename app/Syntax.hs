module Syntax where

import Data.List (intersperse)

type Name = String

data Match = Match
  { matchName :: Name,
    matchPats :: [Pat],
    matchRhs :: Expr
  }

instance Show Match where
  show :: Match -> String
  show (Match name [] expr) = showString name . showString " = " $ show expr
  show m =
    showString (matchName m) . showChar ' '
      . shows (matchPats m)
      . showString " = "
      $ show (matchRhs m)

newtype Def = Def {defMatches :: [Match]}

instance Show Def where
  show :: Def -> String
  show def = unlines $ map show (defMatches def)

infix 9 :$

data Expr = Var Name | Con Name | Expr :$ Expr

instance Show Expr where
  showsPrec :: Int -> Expr -> ShowS
  showsPrec _ (Var name) = showString name
  showsPrec _ (Con name) = showString name
  showsPrec p (x :$ y) =
    showParen (p > 9) $
      showsPrec 9 x . showChar ' ' . showsPrec 10 y

data Pat = PVar Name | PApp Name [Pat]

instance Show Pat where
  showsPrec :: Int -> Pat -> ShowS
  showsPrec _ (PVar name) = showString name
  showsPrec _ (PApp name []) = showString name
  showsPrec p (PApp name pats) =
    showParen (p > 9) $
      showString name . showChar ' ' . shows pats

instance {-# OVERLAPPING #-} Show [Pat] where
  show :: [Pat] -> String
  show pats = foldr ($) "" (intersperse (showChar ' ') (map (showsPrec 10) pats))

newtype Prog = Prog {progDefs :: [Def]} deriving (Show)
