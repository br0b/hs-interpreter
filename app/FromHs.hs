-- TODO: Restrict.
module FromHs where

import qualified Control.Monad as Monad
import Control.Monad.RWS (RWS)
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.State as State
import Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as Writer
import Language.Haskell.ParseMonad (ParseResult (ParseFailed, ParseOk))
import Language.Haskell.Parser (parseModule)
import Language.Haskell.Syntax
  ( HsDecl (HsFunBind, HsPatBind),
    HsExp (HsApp, HsCon, HsParen, HsVar),
    HsMatch (HsMatch),
    HsModule (HsModule),
    HsName (HsIdent),
    HsPat (HsPApp, HsPParen, HsPVar),
    HsQName (UnQual),
    HsRhs (HsUnGuardedRhs),
  )
import Syntax (Def (Def), Expr (Con, Var, (:$)), Match (Match), Name, Pat (PApp, PVar), Prog (Prog))

type Parser = RWS () [Def] Bool

fromHsString :: String -> Prog
fromHsString str = case RWS.execRWS (fromParseResult (parseModule str)) () False of
  (True, defs) -> Prog defs
  (False, _) -> error "No main combinator."

fromParseResult :: ParseResult HsModule -> Parser ()
fromParseResult (ParseOk m) = fromHsModule m
fromParseResult (ParseFailed _ msg) = error msg

fromHsModule :: HsModule -> Parser ()
fromHsModule (HsModule _ _ _ _ binds) = mapM_ fromHsDecl binds

fromHsDecl :: HsDecl -> Parser ()
fromHsDecl (HsFunBind matches) =
  Writer.tell [Def $ Writer.execWriter (mapM_ fromHsMatch matches)]
fromHsDecl (HsPatBind _ (HsPVar (HsIdent name)) (HsUnGuardedRhs rhs) _) = do
  Monad.when (name == "main") (State.put True)
  Writer.tell [Def [Match name [] (fromHsExp rhs)]]
fromHsDecl x = error ("Incorrect combinator definition:" ++ show x)

fromHsMatch :: HsMatch -> Writer [Match] ()
fromHsMatch (HsMatch _ (HsIdent name) pats (HsUnGuardedRhs rhs) _) = do
  Monad.when (name == "main") (error "Main combinator has arguments.")
  Writer.tell [Match name (map fromHsPat pats) (fromHsExp rhs)]
fromHsMatch _ = error "Incorrect match definition."

fromHsPat :: HsPat -> Pat
fromHsPat (HsPVar (HsIdent name)) = PVar name
fromHsPat (HsPApp name pats) = PApp (fromHsUnQual name) (map fromHsPat pats)
fromHsPat (HsPParen x) = fromHsPat x
fromHsPat x = error ("Incorrect combinator argument in definition:" ++ show x)

fromHsExp :: HsExp -> Expr
fromHsExp (HsCon name) = Con $ fromHsUnQual name
fromHsExp (HsVar name) = Var $ fromHsUnQual name
fromHsExp (HsApp x (HsParen (HsApp y z))) = fromHsExp x :$ (fromHsExp y :$ fromHsExp z)
fromHsExp (HsApp x y) = fromHsExp x :$ fromHsExp y
fromHsExp (HsParen x) = fromHsExp x
fromHsExp x = error ("Incorrect combinator expression definition:" ++ show x)

fromHsUnQual :: HsQName -> Name
fromHsUnQual (UnQual (HsIdent x)) = x
fromHsUnQual x = error ("Incorrect variable definition:" ++ show x)
