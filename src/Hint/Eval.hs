module Hint.Eval (

      interpret, as, infer,
      eval ,parens)

where

import qualified GHC.Exts ( unsafeCoerce# )

import Data.Typeable hiding ( typeOf )
import qualified Data.Typeable ( typeOf )
import System.Random (randomIO)
import Control.Monad.Trans ( liftIO )
import Control.Monad.State ( runStateT, gets, modify )

import Hint.Base
import Hint.Context
import Hint.Parsers
import Hint.Sandbox
import Hint.Util

import qualified Hint.Compat as Compat


-- | Convenience functions to be used with @interpret@ to provide witnesses.
--   Example:
--
--   * @interpret \"head [True,False]\" (as :: Bool)@
--
--   * @interpret \"head $ map show [True,False]\" infer >>= flip interpret (as :: Bool)@
as, infer :: Typeable a => a
as    = undefined
infer = undefined

-- | Evaluates an expression, given a witness for its monomorphic type.
interpret :: (MonadInterpreter m, Typeable a) => String -> a -> m a
interpret expr wit = let type_rep = Data.Typeable.typeOf wit
                     in unsafeInterpret expr (Right type_rep)


unsafeInterpret :: (MonadInterpreter m) => String -> (Either String TypeRep) -> m a
unsafeInterpret expr strOrRep = sandboxed go expr
  where go e =
         do -- First, make sure the expression has no syntax errors,
            -- for this is the only way we have to "intercept" this
            -- kind of errors
            failOnParseError parseExpr e
            --
            type_str <- either return mkTypeRepAbsolute strOrRep
                
            let expr_typesig = concat [parens e, " :: ", type_str]
            expr_val <- mayFail $ runGhc1 Compat.compileExpr expr_typesig
            --
            return (GHC.Exts.unsafeCoerce# expr_val :: a)

-- | @eval expr@ will evaluate @show expr@.
--  It will succeed only if @expr@ has type t and there is a 'Show'
--  instance for t.
eval :: MonadInterpreter m => String -> m String
eval expr = do in_scope_show   <- support_show
               in_scope_String <- support_String
               let show_expr = unwords [in_scope_show, parens expr]
               unsafeInterpret show_expr (Left in_scope_String)

mkTypeRepAbsolute :: MonadInterpreter m => TypeRep -> m String
mkTypeRepAbsolute rep = do
  (type_rep, name_table) <- runStateT (procRep rep) []
  addImportsQ [(mod_name, Just pm) | (mod_name, pm) <- name_table]
  return $ show type_rep
  where
    isFun con = con == (typeRepTyCon $ Data.Typeable.typeOf (succ :: Int -> Int))
    isTuple con = tyConModule con == "GHC.Tuple"
    isList con = con == (typeRepTyCon $ Data.Typeable.typeOf [()])
    procRep in_rep = do
      let (con, args) = splitTyConApp in_rep
      new_con <- procCon con
      new_args <- mapM procRep args
      return $ mkTyConApp new_con new_args
    procCon con
      | isFun con || isTuple con || isList con = return con
      | otherwise = do
        let mod_name = tyConModule con
        mn <- gets $ lookup mod_name
        pm <- maybe (mkPhantomName mod_name) return mn
        return $ mkTyCon3 "" "" $ pm ++ "." ++ tyConName con
    mkPhantomName mod_name = do
        n <- liftIO randomIO
        let mod_str = concat [map (\c -> if c == '.' then '_' else c) mod_name, "_", show $ abs (n :: Int)]
        modify ((mod_name, mod_str) :)
        return mod_str

-- | Conceptually, @parens s = \"(\" ++ s ++ \")\"@, where s is any valid haskell
-- expression. In practice, it is harder than this.
-- Observe that if @s@ ends with a trailing comment, then @parens s@ would
-- be a malformed expression. The straightforward solution for this is to
-- put the closing parenthesis in a different line. However, now we are
-- messing with the layout rules and we don't know where @s@ is going to
-- be used!
-- Solution: @parens s = \"(let {foo =\n\" ++ s ++ \"\\n ;} in foo)\"@ where @foo@ does not occur in @s@
parens :: String -> String
parens s = concat ["(let {", foo, " =\n", s, "\n",
                    "                     ;} in ", foo, ")"]
    where foo = safeBndFor s
