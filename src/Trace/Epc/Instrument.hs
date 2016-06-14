{-# LANGUAGE TupleSections #-}
module Trace.Epc.Instrument where

import AST.Declaration (Decl(Decl), Declaration(Definition))
import AST.Module
    ( ImportMethod(ImportMethod), Module(Module), UserImport(UserImport)
    , Header(Header)
    , body, header, imports, name, exports
    )
import AST.Module.Name (Raw)
import AST.V0_16
    ( Commented(Commented), IntRepresentation(DecimalInt), Literal(IntNum)
    , KeywordCommented(KeywordCommented)
    )
import AST.Variable (Listing(..), Ref(VarRef, OpRef), Value(Value, Union))
import Control.Monad.Reader (Reader, ask)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Reporting.Region (Position(Position))
import Trace.Hpc.Mix (BoxLabel(TopLevelBox), MixEntry)

import qualified AST.Expression as E
import qualified Reporting.Annotation as A
import qualified Data.Map as M

type Env = (String, [MixEntry])

allSeeingEye :: [Module] -> M.Map String Raw
allSeeingEye ms = M.fromList (extractMapping =<< fmap header ms)

extractMapping :: Header -> [(String, Raw)]
extractMapping Header {name = Commented _ names _, exports = KeywordCommented _ _ exps} =
    valueToTuple names <$> exportedNames exps

exportedNames :: Listing Value -> [Commented Value]
exportedNames (ExplicitListing funs _) =
    funs
exportedNames (OpenListing _) =
    []
exportedNames ClosedListing =
    []

valueToTuple :: Raw -> Commented Value -> (String, Raw)
valueToTuple raw (Commented _ (Value (VarRef str)) _) = (str, raw)
valueToTuple raw (Commented _ (Value (OpRef str)) _) = (str, raw)
valueToTuple raw (Commented _ (Union (str, _) _) _) = (str, raw)

instrumentModule :: Module -> Reader Env Module
instrumentModule m@Module {body = decls, imports = imps, header = headr} = do
    body' <- traverse instrumentDecl decls
    let imports' = UserImport (at0 (([], ["Coverage"]), ImportMethod Nothing ([], ([], ClosedListing)))):imps
    let header' = headr { name = ("Instrumented":) <$> name headr }
    pure $ m { body = body'
             , imports = imports'
             , header = header'
             }

instrumentDecl :: Decl -> Reader Env Decl
instrumentDecl (Decl (A.A region declaration)) =
    Decl . A.A region <$> instrumentDeclaration declaration
instrumentDecl decl = pure decl

instrumentDeclaration :: Declaration -> Reader Env Declaration
instrumentDeclaration (Definition pat commentedPats comments expr) =
    Definition pat commentedPats comments <$> instrumentExpr expr
instrumentDeclaration declaration = pure declaration

instrumentExpr :: E.Expr -> Reader Env E.Expr
instrumentExpr (A.A region expr) = A.A region <$> instrumentExpr' expr

instrumentExpr' :: E.Expr' -> Reader Env E.Expr'
instrumentExpr' (E.Var (VarRef var)) = do
    (str, entries) <- ask
    let instrumented =
          E.App
              (varRef "Coverage.tick")
              [ ([], varRef ("\"" ++ str ++ "\""))
              , ([], literalInt . fromMaybe (-1) $ findIndex (isMixEntry var) entries)
              , ([], varRef var)
              ]
              False
    pure $ E.Parens (Commented [] (at0 instrumented) [])
instrumentExpr' (E.Unary E.Negative expr) =
    E.Unary E.Negative <$> instrumentExpr expr
instrumentExpr' (E.Binops expr ops bool) =
    E.Binops <$> instrumentExpr expr
             <*> traverse (\(pre, op, post, expr') -> (,,,) <$> pure pre <*> pure op <*> pure post <*> instrumentExpr expr') ops
             <*> pure bool
instrumentExpr' (E.Parens (Commented pre expr post)) =
    (\expr' -> E.Parens $ Commented pre expr' post) <$> instrumentExpr expr
instrumentExpr' expr = pure expr

isMixEntry :: String -> MixEntry -> Bool
isMixEntry str (_, TopLevelBox strs@(_:_)) = str == last strs
isMixEntry _ _ = False

at0 :: a -> A.Located a
at0 = A.at (Position 0 0) (Position 0 0)

varRef :: String -> E.Expr
varRef = at0 . E.Var . VarRef

literalInt :: Int -> E.Expr
literalInt n = at0 $ E.Literal (IntNum n DecimalInt)
