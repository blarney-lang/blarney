{-|
Module      : Namer
Description : Namer plugin for Blarney
Copyright   : (c) Alexandre Joannou, 2019
License     : MIT
Maintainer  : alexandre.joannou@gmail.com
Stability   : experimental

This module defines a Namer plugin for Blarney to preserve names from Blarney
source code down to generated code.

-}
module BlarneyPlugins.Namer (plugin) where

import Control.Monad.IO.Class ( liftIO )

-- ghc
import qualified Convert as GHC
import qualified CoreUtils
import qualified Desugar as GHC
import qualified Finder as GHC
import qualified GHC
import qualified GhcPlugins as GHC
import qualified HsExpr as Expr
import qualified IfaceEnv as GHC
import qualified PrelNames as GHC
import qualified RnExpr as GHC
import qualified TcEnv as GHC
import qualified TcEvidence as GHC
import qualified TcExpr as GHC
import qualified TcHsSyn as GHC
import qualified TcRnMonad as GHC
import qualified TcSimplify as GHC
import qualified TcType as GHC
import qualified Type

-- syb
import qualified Data.Generics as SYB

debug = liftIO . putStrLn

plugin :: GHC.Plugin
plugin = GHC.defaultPlugin { GHC.typeCheckResultAction = install }

showBinds :: Expr.ExprLStmt GHC.GhcTc -> GHC.TcM ( Expr.ExprLStmt GHC.GhcTc )
showBinds e@(GHC.L _ (Expr.BindStmt _ pat body expr0 expr1)) = do

  hs_env <- GHC.getTopEnv
  (_, mbe) <- liftIO (GHC.deSugarExpr hs_env body)

  blarneyModuleModule <-
    liftIO ( GHC.findImportedModule hs_env ( GHC.mkModuleName "Blarney.Module" ) Nothing )

  case blarneyModuleModule of
    GHC.Found _ m -> do
      moduleName <- GHC.lookupOrig m ( GHC.mkTcOcc "Module" )
      debug $ GHC.showPpr GHC.unsafeGlobalDynFlags moduleName
      debug $ GHC.showPpr GHC.unsafeGlobalDynFlags (GHC.occName moduleName)
      case CoreUtils.exprType <$> mbe of
        Nothing -> return ()
        Just t  -> do debug $ GHC.showPpr GHC.unsafeGlobalDynFlags t
                      debug $ GHC.showPpr GHC.unsafeGlobalDynFlags (Type.splitTyConApp_maybe t)
                      case Type.splitTyConApp_maybe t of
                        Just (tyC, _) -> do
                          if (GHC.tyConName tyC) == (moduleName) then debug "===============================> HOORAYYYY"
                          else do debug $ "======================================> WEIRD"
                                  debug $ GHC.showPpr GHC.unsafeGlobalDynFlags tyC
                                  debug $ GHC.showPpr GHC.unsafeGlobalDynFlags (GHC.occName $ GHC.tyConName tyC)
                        _ -> debug "===============================> SO SAD"
    _ -> return ()

  debug $ GHC.showPpr GHC.unsafeGlobalDynFlags e
  debug $ GHC.showPpr GHC.unsafeGlobalDynFlags pat
  debug $ GHC.showPpr GHC.unsafeGlobalDynFlags body
  debug $ GHC.showPpr GHC.unsafeGlobalDynFlags expr0
  debug $ GHC.showPpr GHC.unsafeGlobalDynFlags expr1
  debug "--------------------------------------------------"
  return e
showBinds e = return e


install :: [GHC.CommandLineOption] -> GHC.ModSummary -> GHC.TcGblEnv -> GHC.TcM GHC.TcGblEnv
install opts modS tcGblEnv = do
  tcg_binds <-
    SYB.mkM showBinds `SYB.everywhereM` GHC.tcg_binds tcGblEnv
  return tcGblEnv
