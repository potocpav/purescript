module Language.PureScript.CodeGen.BLC
  ( moduleToBlc
  ) where


import Data.Text (Text, pack)
-- import Control.Monad.Supply.Class

import Prelude.Compat
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply.Class

import Language.PureScript.CoreFn
import Language.PureScript.Crash
import Language.PureScript.Names
import Language.PureScript.Options

import Language.PureScript.CodeGen.BLC.AST (AST, Definition(..))
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
  -> m [Definition]
moduleToBlc (Module _ coms mn _ imps exps foreigns decls) foreign_ =
  rethrow (addHint (ErrorInModule mn)) $ do
    blcDefs <- mapM bindToBlc decls
    return $ concat blcDefs

  where

  bindToBlc :: Bind Ann -> m [Definition]
  bindToBlc (NonRec ann ident val) = return <$> nonRecToBlc ann ident val
  bindToBlc (Rec vals) = undefined

  nonRecToBlc :: Ann -> Ident -> Expr Ann -> m Definition
  nonRecToBlc (ss, _, _, _) ident val = do
    body <- valueToBlc val
    return $ Definition (identToBlc ident) body

  identToBlc :: Ident -> Text
  identToBlc (Ident name) = name
  identToBlc (GenIdent _ _) = internalError "GenIdent in identToBlc"
  identToBlc UnusedIdent = "$__unused"

  valueToBlc :: Expr Ann -> m AST
  valueToBlc (Literal _ _) =
    return $ AST.Unimpl "Literal"
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
  valueToBlc (Case _ _ _) =
    return $ AST.Unimpl "Case"
  valueToBlc (Let _ _ _) =
    return $ AST.Unimpl "Let"

  varToBlc :: Qualified Ident -> AST
  varToBlc (Qualified Nothing ident) = AST.Var $ identToBlc ident
  varToBlc qual = AST.Var $ showQualified identToBlc qual
  -- valueToBlc _ = undefined --return $ pack $ show e
