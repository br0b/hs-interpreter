module FromHs (fromHsString) where

import Language.Haskell.ParseMonad (ParseResult (ParseFailed, ParseOk))
import Language.Haskell.Parser (parseModule)
import Language.Haskell.Syntax (HsDecl (HsFunBind, HsPatBind), HsExp (HsApp, HsCon, HsParen, HsVar), HsMatch (HsMatch), HsModule (HsModule), HsName (HsIdent), HsPat (HsPVar), HsQName (UnQual), HsRhs (HsUnGuardedRhs))
import Syntax (Def (Def), Expr (Var, (:$)), Pat, Prog (Prog, progDefs))
import Utils (hasDuplicates)

multipleDefErr :: a
multipleDefErr = error "Multible definitions for the same combinator"

fromHsString :: String -> Prog
fromHsString str = createProg $ fromParseResult $ parseModule str

createProg :: [Def] -> Prog
createProg defs = let names = map (\(Def name _ _) -> name) defs
  in if hasDuplicates names
    then multipleDefErr
    else Prog { progDefs = defs }

fromParseResult :: ParseResult HsModule -> [Def]
fromParseResult (ParseOk m) = fromHsModule m
fromParseResult (ParseFailed _ msg) = error msg

fromHsModule :: HsModule -> [Def]
fromHsModule (HsModule _ _ _ _ binds) = map fromHsDecl binds

-- [HsMatch _ (HsIdent name) pats (HsUnGuardedRhs rhs) _]
fromHsDecl :: HsDecl -> Def
fromHsDecl (HsFunBind matches) = case matches of
  [HsMatch _ (HsIdent name) pats (HsUnGuardedRhs rhs) _] -> fromParsedHsDecl name pats rhs
  _ -> multipleDefErr
fromHsDecl (HsPatBind _ (HsPVar (HsIdent name)) (HsUnGuardedRhs rhs) _) =
  fromParsedHsDecl name [] rhs
fromHsDecl x = error ("Incorrect combinator definition:" ++ show x)

fromParsedHsDecl :: String -> [HsPat] -> HsExp -> Def
fromParsedHsDecl name pats rhs =
  let pats' = map fromHsPat pats
   in if hasDuplicates pats'
    then error ("Combinator " ++ name ++ " has repeating parameter names.")
    else Def name pats' (fromHsExp rhs)

fromHsPat :: HsPat -> Pat
fromHsPat (HsPVar (HsIdent pat)) = pat
fromHsPat _ = error "Incorrect combinator argument in definition:"

fromHsExp :: HsExp -> Expr
fromHsExp (HsCon name) = fromHsUnQual name
fromHsExp (HsVar name) = fromHsUnQual name
fromHsExp (HsApp x (HsParen (HsApp y z))) = fromHsExp x :$ (fromHsExp y :$ fromHsExp z)
fromHsExp (HsApp x y) = fromHsExp x :$ fromHsExp y
fromHsExp (HsParen x) = fromHsExp x
fromHsExp x = error ("Incorrect combinator expression definition:" ++ show x)

fromHsUnQual :: HsQName -> Expr
fromHsUnQual (UnQual (HsIdent x)) = Var x
fromHsUnQual x = error ("Incorrect variable definition:" ++ show x)