module Hint.Reflection (

      ModuleElem(..), Id, name, children,
      getModuleExports,

)

where

import Data.List
import Data.Maybe

import Hint.Base
import qualified Hint.GHC as GHC
import qualified Hint.Compat as Compat

-- | An Id for a class, a type constructor, a data constructor, a binding, etc
type Id = String

data ModuleElem = Fun Id | Class Id [Id] | Data Id [Id]
  deriving (Read, Show, Eq)

name :: ModuleElem -> Id
name (Fun f)     = f
name (Class c _) = c
name (Data d _)  = d

children :: ModuleElem -> [Id]
children (Fun   _)     = []
children (Class _ ms)  = ms
children (Data  _ dcs) = dcs


-- | Gets an abstract representation of all the entities exported by the module.
--   It is similar to the @:browse@ command in GHCi.
getModuleExports :: MonadInterpreter m => ModuleName -> m [ModuleElem]
getModuleExports mn =
    do module_  <- findModule mn
       mod_info <- mayFail $ runGhc1 GHC.getModuleInfo module_
       exports  <- mapM (runGhc1 GHC.lookupName) (GHC.modInfoExports mod_info)
       dflags   <- runGhc GHC.getSessionDynFlags
       --
       return $ asModElemList dflags (catMaybes exports)

asModElemList :: GHC.DynFlags -> [GHC.TyThing] -> [ModuleElem]
asModElemList df xs = concat [
                        cs',
                        ts',
                        ds \\ (concatMap (map Fun . children) ts'),
                        fs \\ (concatMap (map Fun . children) cs')
                      ]
    where (cs,ts,ds,fs) =
           (
#if __GLASGOW_HASKELL__ < 704
             [asModElem df c | c@GHC.AClass{}   <- xs],
             [asModElem df t | t@GHC.ATyCon{}   <- xs],
#else
             [asModElem df c | c@(GHC.ATyCon c')   <- xs, GHC.isClassTyCon c'],
             [asModElem df t | t@(GHC.ATyCon c')   <- xs, (not . GHC.isClassTyCon) c'],
#endif
#if __GLASGOW_HASKELL__ < 708
             [asModElem df d | d@GHC.ADataCon{} <- xs],
#else
             [asModElem df d | d@(GHC.AConLike (GHC.RealDataCon{})) <- xs],
#endif
             [asModElem df f | f@GHC.AnId{}     <- xs]
           )
          cs' = [Class n $ filter (alsoIn fs) ms  | Class n ms  <- cs]
          ts' = [Data  t $ filter (alsoIn ds) dcs | Data  t dcs <- ts]
          alsoIn es = (`elem` (map name es))


asModElem :: GHC.DynFlags -> GHC.TyThing -> ModuleElem
asModElem df (GHC.AnId f)      = Fun $ getUnqualName df f
#if __GLASGOW_HASKELL__ < 708
asModElem df (GHC.ADataCon dc) = Fun $ getUnqualName df dc
#else
asModElem df (GHC.AConLike (GHC.RealDataCon dc)) = Fun $ getUnqualName df dc
#endif
#if __GLASGOW_HASKELL__ < 704
asModElem df(GHC.ATyCon tc)   = Data  (getUnqualName df tc)
                                      (map (getUnqualName df) $ GHC.tyConDataCons tc)
asModElem df (GHC.AClass c)    = Class (getUnqualName df c)
                                      (map (getUnqualName df) $ GHC.classMethods c)
#else
asModElem df (GHC.ATyCon tc)   =
  if GHC.isClassTyCon tc
  then Class (getUnqualName df tc) (map (getUnqualName df) $ (GHC.classMethods . fromJust . GHC.tyConClass_maybe) tc)
  else Data  (getUnqualName df tc) (map (getUnqualName df) $ GHC.tyConDataCons tc)
asModElem _ _ = error "asModElem: can't happen!"
#endif

getUnqualName :: GHC.NamedThing a => GHC.DynFlags -> a -> String
getUnqualName dfs = Compat.showSDocUnqual dfs . GHC.pprParenSymName
