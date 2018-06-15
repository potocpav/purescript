module Language.PureScript.CodeGen.BLC
  ( moduleToBlc
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Foldable (foldlM)
-- import Control.Monad.Supply.Class

import Prelude.Compat
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply.Class

import Language.PureScript.CoreFn
import Language.PureScript.Crash
import Language.PureScript.Names
import Language.PureScript.PSString (decodeString)
import Language.PureScript.Options

import Language.PureScript.CodeGen.BLC.AST (AST)
import qualified Language.PureScript.CodeGen.BLC.AST as AST
import Language.PureScript.Errors (ErrorMessageHint(..), SimpleErrorMessage(..),
                                   MultipleErrors(..), rethrow, errorMessage,
                                   errorMessage', rethrowWithPosition, addHint)

-- | Generate code in the simplified BLC intermediate representation for all declarations in a
-- module.
moduleToBlc
  :: forall m
   . (Monad m, MonadReader Options m, MonadSupply m, MonadError MultipleErrors m)
  => Module Ann
  -> Maybe AST
  -> m AST.BLC
moduleToBlc (Module _ coms mn _ imps exps foreigns decls) foreign_ =
  rethrow (addHint (ErrorInModule mn)) $ do
    blcDefs <- mapM bindToDef decls
    let imports = (AST.Import . moduleNameToText . snd) <$> imps
    return $ AST.BLC (moduleNameToText mn) imports (concat blcDefs)

  where

  bindToDef :: Bind Ann -> m [AST.Definition]
  bindToDef (NonRec ann ident val) = return <$> nonRecToDef ann ident val
  bindToDef (Rec vals) = undefined

  nonRecToDef :: Ann -> Ident -> Expr Ann -> m AST.Definition
  nonRecToDef _ ident val = do
    body <- valueToBlc val
    return $ AST.Definition (identToBlc ident) body

  bindToBlc :: AST -> Bind Ann -> m AST
  bindToBlc body (NonRec ann ident val) = nonRecToBlc ann ident val body
  bindToBlc _ (Rec _) = undefined

  nonRecToBlc :: Ann -> Ident -> Expr Ann -> AST -> m AST
  nonRecToBlc _ ident val body = do
    val' <- valueToBlc val
    return $ AST.App (AST.Abs (identToBlc ident) body) val'

  identToBlc :: Ident -> Text
  identToBlc (Ident name) = name
  identToBlc (GenIdent _ _) = internalError "GenIdent in identToBlc"
  identToBlc UnusedIdent = "$__unused"

  valueToBlc :: Expr Ann -> m AST
  valueToBlc (Literal _ l) =
    literalToValueBlc l
  valueToBlc (Constructor _ _ _ _) =
    return $ AST.Unimpl "Constructor"
  valueToBlc (Accessor _ _ _) =
    return $ AST.Unimpl "Accessor"
  valueToBlc (ObjectUpdate _ _ _) =
    return $ AST.Unimpl "ObjectUpdate"
  valueToBlc (Abs _ ident expr) =
    AST.Abs (identToBlc ident) <$> valueToBlc expr
  valueToBlc (App _ expr1 expr2) = do
    e1 <- valueToBlc expr1
    e2 <- valueToBlc expr2
    return $ AST.App e1 e2
  valueToBlc (Var _ var) =
    return $ varToBlc var
  valueToBlc c@(Case _ _ _) =
    return $ AST.Unimpl $ T.pack $ show c
  valueToBlc (Let _ ds val) = do
    body <- valueToBlc val
    ds' <- foldlM bindToBlc body ds
    return $ ds'

  literalToValueBlc :: Literal (Expr Ann) -> m AST
  literalToValueBlc (NumericLiteral (Left i)) = return $ AST.NumericLiteral (Left i)
  literalToValueBlc (NumericLiteral (Right n)) = return $ AST.NumericLiteral (Right n)
  literalToValueBlc (StringLiteral s) = return $ AST.StringLiteral (fromJust $ decodeString s)
  literalToValueBlc (BooleanLiteral b) = return $ AST.BooleanLiteral b
  literalToValueBlc (ArrayLiteral as) = AST.ArrayLiteral <$> (mapM valueToBlc as)

  varToBlc :: Qualified Ident -> AST
  varToBlc (Qualified Nothing ident) = AST.Var $ identToBlc ident
  varToBlc (Qualified (Just mn) ident) = AST.Var $
    moduleNameToText mn <> "__" <> identToBlc ident

  moduleNameToText :: ModuleName -> Text
  moduleNameToText (ModuleName mn) = T.intercalate "__" (runProperName <$> mn)
