{-# LANGUAGE LambdaCase #-}

-- TODO: Restrict.
module Reduce where

import qualified Control.Monad as Monad
import Control.Monad.RWS (RWS)
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Control.Monad.Writer as Writer
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Debug.Trace
import SnocList (SnocList)
import qualified SnocList
import Syntax (Def, Expr, Match, Name, Pat, Prog)
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

-- Writer monad will be used to build an RPah
type Reduction = MaybeT (RWS DefMap (SnocList String) RData)

type RPath = [String]

reduce :: Prog -> RPath
reduce prog =
  SnocList.toList $
    snd $ RWS.evalRWS (MaybeT.runMaybeT rpath) (buildDefMap prog) (initRData (Syntax.Var "main") 30)

rpath :: Reduction ()
rpath = apath >> Monad.void rfull

rfull :: Reduction ()
rfull = do
  loc <- State.gets loc
  trace ("-- rfull at " ++ show loc) $ return ()
  case expr loc of
    _ Syntax.:$ _ -> leftmost >> rfull
    Syntax.Con _ -> rargs
    Syntax.Var name ->
      rmatchInit name >>= \case
        Just rmatch' -> do
          trace "-- Calling rmatch." $ return ()
          Monad.void rmatch'
          trace "-- Calling rfull." $ return ()
          rfull
        Nothing -> rargs

rargs :: Reduction ()
rargs = do
  loc <- State.gets loc
  case cxt loc of
    Top -> return ()
    L _ _ -> rightSibling >> apath >> rfull >> parent >> rargs
    R _ _ -> leftSibling

rmatchInit :: Name -> Reduction (Maybe (Reduction Bool))
rmatchInit name = do
  trace ("-- Matching " ++ show name) $ return ()
  Reader.ask >>= \dm -> case Map.lookup name dm of
    Just def -> return $ Just $ rmatch Map.empty (Syntax.defMatches def)
    Nothing -> return Nothing

rmatch :: VarMap -> [Match] -> Reduction Bool
rmatch _ [] = do
  trace "-- Match failed." $ return ()
  leftmost
  return False
rmatch vm (m : ms) = do
  loc <- State.gets loc
  case Syntax.matchPats m of
    pat : pats' -> do
      trace ("-- Matching using " ++ show m) $ return ()
      trace ("-- Starting from " ++ show loc) $ return ()
      case cxt loc of
        L _ _ -> do
          rightSibling
          rpat vm pat >>= \case
            Just vm' -> parent >> rmatch vm' (m {Syntax.matchPats = pats'} : ms)
            Nothing -> leftmost >> rmatch Map.empty ms
        _ -> leftmost >> rmatch Map.empty ms
    [] -> do
      trace ("-- Reducing " ++ show loc ++ " to " ++ show (Syntax.matchRhs m)) $ return ()
      setLoc $ loc {expr = subst vm (Syntax.matchRhs m)}
      apath
      leftmost
      return True

subst :: VarMap -> Expr -> Expr
subst vm expr = case expr of
  l Syntax.:$ r -> subst vm l Syntax.:$ subst vm r
  Syntax.Var name -> Maybe.fromMaybe expr $ Map.lookup name vm
  Syntax.Con name -> Syntax.Con name

rpat :: VarMap -> Pat -> Reduction (Maybe VarMap)
rpat vm pat = do
  loc <- State.gets loc
  trace ("-- Matching pattern = " ++ show pat) $ return ()
  trace ("-- at " ++ show loc) $ return ()
  trace ("-- at " ++ show (expr loc)) $ return ()
  case pat of
    Syntax.PVar name -> do
      leftSibling
      return $ Just $ Map.insert name (expr loc) vm
    Syntax.PApp name pats -> do
      case expr loc of
        _ Syntax.:$ _ -> leftmost >> rpat vm pat
        Syntax.Con name' -> do
          if name == name'
            then rpatcon vm name pats
            else rfail
        Syntax.Var name' ->
          rmatchInit name' >>= \case
            Just rmatch' ->
              rmatch' >>= \case
                True -> rpat vm pat
                False -> rfail
            Nothing -> rfail

rpatcon :: VarMap -> Name -> [Pat] -> Reduction (Maybe VarMap)
rpatcon vm name pats = do
  loc <- State.gets loc
  trace ("-- Matching con = " ++ show name ++ ";" ++ show pats) $ return ()
  trace ("-- at " ++ show loc) $ return ()
  case cxt loc of
    L _ _ -> case pats of
      pat : pats' -> do
        rightSibling
        rpat vm pat >>= \case
          Just vm' -> parent >> rpatcon vm' name pats'
          Nothing -> rfail
      [] -> rfail
    R _ _ -> if null pats then leftSibling >> return (Just vm) else rfail
    Top ->
      error
        "Reduction rpatcon should never be called"
        "from the top combinator."

-- case pats of
--   pat : pats' -> do
--     rightSibling
--     rpat vm pat >>= \case
--       Just vm' ->
--         case cxt loc of
--           L _ _ -> parent >> rpatcon vm' name pats'
--           _ -> leftSibling >> return (if null pats' then Just vm' else Nothing)
--       Nothing -> rfail
--   [] -> rfail

rfail :: Reduction (Maybe a)
rfail = rightmost >> leftSibling >> return Nothing

-- Returns true iff there are more steps left.
apath :: Reduction ()
apath = do
  -- Append the loc to the reduction path.
  State.gets loc >>= Writer.tell . pure . show
  -- Lower the maximal number of steps left.
  newStepCap <- State.gets $ (\x -> x - 1) . stepCap
  State.modify $ \rd -> rd {stepCap = newStepCap}
  if newStepCap > 0 then return () else MaybeT $ return Nothing

setLoc :: Loc -> Reduction ()
setLoc loc = State.modify $ \rd -> rd {loc = loc}

leftmost :: Reduction ()
leftmost =
  State.gets loc >>= \loc -> case expr loc of
    l Syntax.:$ r -> setLoc (Loc l (L (cxt loc) r)) >> leftmost
    _ -> return ()

rightmost :: Reduction ()
rightmost =
  State.gets loc >>= \loc -> case cxt loc of
    L p rexpr -> setLoc (Loc (expr loc Syntax.:$ rexpr) p) >> rightmost
    _ -> return ()

leftSibling :: Reduction ()
leftSibling =
  State.gets loc >>= \loc -> case cxt loc of
    R p lexpr -> setLoc $ Loc lexpr (L p (expr loc))
    _ -> error "Called not from right sibling."

rightSibling :: Reduction ()
rightSibling =
  State.gets loc >>= \loc -> case cxt loc of
    L p rexpr -> setLoc $ Loc rexpr (R p (expr loc))
    _ -> error "Called not from left sibling."

parent :: Reduction ()
parent =
  State.gets loc >>= \loc -> case cxt loc of
    L p r -> setLoc $ Loc (expr loc Syntax.:$ r) p
    R p l -> setLoc $ Loc (l Syntax.:$ expr loc) p
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
initRData expr stepCap = RData {loc = Loc expr Top, stepCap = stepCap}
