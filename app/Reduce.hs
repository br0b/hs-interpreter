{-# LANGUAGE LambdaCase #-}

module Reduce where

import Control.Monad (MonadPlus (mzero))
import qualified Control.Monad as Monad
import Control.Monad.RWS (RWS)
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Control.Monad.Writer as Writer
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Debug.Trace (traceM)
import SnocList (SnocList)
import qualified SnocList
import Syntax (Def, Expr, Name, Pat, Prog)
import qualified Syntax

type DefMap = Map Name Def

type VarMap = Map Name Expr

data Cxt = Top | L Cxt Expr | R Cxt Expr

data Loc = Loc {expr :: Expr, cxt :: Cxt}

instance Show Loc where
  show :: Loc -> String
  show (Loc expr Top) = showWithBrackets expr ""
  show (Loc expr (L p s)) = showLoc (showWithBrackets expr . showChar ' ' . showsPrec 10 s) p
  show (Loc expr (R p s)) = showLoc (shows s . showChar ' ' . showWithBrackets expr) p

showWithBrackets :: Expr -> ShowS
showWithBrackets expr = showChar '{' . shows expr . showChar '}'

showLoc :: ShowS -> Cxt -> String
showLoc exprs Top = exprs ""
showLoc exprs (L p s) = showLoc (exprs . showChar ' ' . showsPrec 10 s) p
showLoc exprs (R p s) = showLoc (shows s . showChar ' ' . showParen True exprs) p

data RData = RData {loc :: Loc, stepCap :: Int}

-- Writer monad will be used to build an RPath.
type Reduction = MaybeT (RWS DefMap (SnocList String) RData)

type RPath = [String]

reduce :: Prog -> RPath
reduce prog =
  SnocList.toList $
    snd $ RWS.evalRWS (MaybeT.runMaybeT rpath) (buildDefMap prog) (initRData (Syntax.Var "main") 31)

rpath :: Reduction ()
rpath = apath >> rfull

-- Keep reducing the expression until the expression is fully reduced or stepCap == 0.
rfull :: Reduction ()
rfull = do
  loc <- State.gets loc
  traceM ("-- rfull at " ++ show loc)
  case expr loc of
    _ Syntax.:$ _ -> leftmost >> rfull
    Syntax.Con _ -> rargs
    Syntax.Var name ->
      rmatch name >>= \case
        True -> rfull
        False -> rargs

-- Call `rfull` on each of the arguments of the current application.
rargs :: Reduction ()
rargs = do
  loc <- State.gets loc
  case cxt loc of
    Top -> return ()
    L _ _ -> rightSibling >> apath >> rfull >> parent >> rargs
    R _ _ -> leftSibling

-- Try to reduce the expression by matching against a definition of a combinator called `defName`.
-- Should be called from the leftmost expression. Will finish in the location it was called from.
rmatch :: Name -> Reduction Bool
rmatch defName = do
  traceM "-- Calling rmatch."
  dm <- Reader.ask
  case Map.lookup defName dm of
    Nothing -> return False
    Just def -> leftmost >> go (Syntax.defMatches def)
      where
        go [] = do
          traceM "-- Match failed."
          leftmost >> return False
        go (m : ms) = goArgs Map.empty m >>= \case
          True -> return True
          False -> go ms
        goArgs variableMap m = do
          loc <- State.gets loc
          case Syntax.matchPats m of
            pat : pats -> do
              traceM ("-- Matching using " ++ show m)
              traceM ("-- Starting from " ++ show loc)
              case cxt loc of
                L _ _ -> do
                  rightSibling
                  rpat variableMap pat >>= \case
                    -- Success, got an updated variable map. Go to the next argument.
                    Just variableMap' -> parent >> goArgs variableMap' (m {Syntax.matchPats = pats})
                    -- The pattern of the application argumnt doesn't match.
                    -- Try to match the next definition arguments.
                    Nothing -> leftmost >> return False
                -- Not enough arguments for this pattern, try again with the next one.
                _ -> leftmost >> return False
            -- All arugments matcheed. Success.
            [] -> do
              traceM ("-- Reducing " ++ show loc ++ " to " ++ show (Syntax.matchRhs m))
              setLoc $ loc {expr = subst variableMap (Syntax.matchRhs m)}
              apath >> leftmost >> return True
        
-- Substitiute the variable names in the expression with the corresponding expression from `VarMap`.
subst :: VarMap -> Expr -> Expr
subst vm expr = case expr of
  l Syntax.:$ r -> subst vm l Syntax.:$ subst vm r
  Syntax.Var name -> Maybe.fromMaybe expr $ Map.lookup name vm
  Syntax.Con name -> Syntax.Con name

-- Try to match this argument to the pattern.
rpat :: VarMap -> Pat -> Reduction (Maybe VarMap)
rpat vm pat = do
  loc <- State.gets loc
  traceM ("-- Matching pattern = " ++ show pat)
  traceM ("-- in " ++ show loc)
  traceM ("-- at " ++ show (expr loc))
  case pat of
    Syntax.PVar name -> leftSibling >> return (Just $ Map.insert name (expr loc) vm)
    Syntax.PApp name pats -> do
      case expr loc of
        _ Syntax.:$ _ -> leftmost >> rpat vm pat
        Syntax.Con name' -> do
          if name == name'
            then rpatcon vm name pats
            else rfail
        Syntax.Var name' ->
          rmatch name' >>= \case
            True -> rpat vm pat
            False -> rfail

-- Try to match combinator argument pattern to a constant.
rpatcon :: VarMap -> Name -> [Pat] -> Reduction (Maybe VarMap)
rpatcon vm name pats = do
  loc <- State.gets loc
  traceM ("-- Matching con = " ++ show name ++ ";" ++ show pats)
  traceM ("-- at " ++ show loc)
  case cxt loc of
    L _ _ -> case pats of
      pat : pats' ->
        rightSibling >> rpat vm pat >>= \case
          Just vm' -> parent >> rpatcon vm' name pats'
          Nothing -> rfail
      [] -> rfail
    R _ _ -> if null pats then leftSibling >> return (Just vm) else rfail
    Top -> error "Reduction rpatcon should never be called from the top combinator."

-- Go one level up the 'call stack' and report error.
rfail :: Reduction (Maybe a)
rfail = rightmost >> leftSibling >> return Nothing

-- Apped the current location to the reduction path.
-- If the step capacity is reached, the return `Nothing`.
apath :: Reduction ()
apath = do
  -- Append the loc to the reduction path.
  State.gets loc >>= Writer.tell . pure . show
  -- Lower the maximal number of steps left.
  newStepCap <- State.gets $ (\x -> x - 1) . stepCap
  State.modify (\rd -> rd {stepCap = newStepCap})
  Monad.when (newStepCap == 0) mzero

setLoc :: Loc -> Reduction ()
setLoc loc = State.modify $ \rd -> rd {loc = loc}

leftmost :: Reduction ()
leftmost =
  State.gets loc >>= \loc -> case expr loc of
    l Syntax.:$ r -> setLoc (Loc {expr = l, cxt = L (cxt loc) r}) >> leftmost
    _ -> return ()

rightmost :: Reduction ()
rightmost =
  State.gets loc >>= \loc -> case cxt loc of
    L p rexpr -> setLoc (Loc {expr = expr loc Syntax.:$ rexpr, cxt = p}) >> rightmost
    _ -> return ()

leftSibling :: Reduction ()
leftSibling =
  State.gets loc >>= \loc -> case cxt loc of
    R p lexpr -> setLoc $ Loc {expr = lexpr, cxt = L p (expr loc)}
    _ -> error "Called not from right sibling."

rightSibling :: Reduction ()
rightSibling =
  State.gets loc >>= \loc -> case cxt loc of
    L p rexpr -> setLoc $ Loc {expr = rexpr, cxt = R p (expr loc)}
    _ -> error "Called not from left sibling."

parent :: Reduction ()
parent =
  State.gets loc >>= \loc -> case cxt loc of
    L p r -> setLoc $ Loc {expr = expr loc Syntax.:$ r, cxt = p}
    R p l -> setLoc $ Loc {expr = l Syntax.:$ expr loc, cxt = p}
    _ -> error "Called from top."

buildDefMap :: Prog -> DefMap
buildDefMap =
  foldr
    ( \def -> case Syntax.defMatches def of
        match : _ -> Map.insert (Syntax.matchName match) def
        _ -> error "Empty matches list"
    )
    Map.empty
    . Syntax.progDefs

initRData :: Expr -> Int -> RData
initRData expr stepCap = RData {loc = Loc {expr = expr, cxt = Top}, stepCap = stepCap}
