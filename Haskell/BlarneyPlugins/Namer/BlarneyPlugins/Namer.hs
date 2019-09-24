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

{-# LANGUAGE ScopedTypeVariables #-}

module BlarneyPlugins.Namer (plugin) where

import Control.Monad.IO.Class ( liftIO )

-- ghc
import qualified Convert as GHC
import qualified CoreUtils
import qualified Desugar as GHC
import qualified Finder as GHC
import qualified GHC
-- TODO faststring
import qualified GhcPlugins as GHC
import qualified HsExpr as Expr
import qualified HsPat as Pat
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
showBinds e@(GHC.L loc (Expr.BindStmt xbind pat body expr0 expr1)) = do

  hs_env <- GHC.getTopEnv
  (_, mbe) <- liftIO (GHC.deSugarExpr hs_env body)

  blarneyModuleModule <-
    liftIO ( GHC.findImportedModule hs_env ( GHC.mkModuleName "Blarney.Module" ) Nothing )

  case blarneyModuleModule of
    GHC.Found _ m -> do
      moduleName <- GHC.lookupOrig m ( GHC.mkTcOcc "Module" )
      --namer <- GHC.lookupOrig m ( GHC.mkVarOcc "withNewName" )
      debug $ GHC.showPpr GHC.unsafeGlobalDynFlags moduleName
      debug $ GHC.showPpr GHC.unsafeGlobalDynFlags (GHC.occName moduleName)
      case CoreUtils.exprType <$> mbe of
        Nothing -> return e
        Just t  -> do debug $ GHC.showPpr GHC.unsafeGlobalDynFlags t
                      debug $ GHC.showPpr GHC.unsafeGlobalDynFlags (Type.splitTyConApp_maybe t)
                      case Type.splitTyConApp_maybe t of
                        Just (tyC, [tyArg]) -> do
                          if (GHC.tyConName tyC) == (moduleName)
                            then do debug "===============================> HOORAYYYY"
                                    namer <- GHC.lookupId =<< GHC.lookupOrig m ( GHC.mkVarOcc "withNewName" )
                                    let f :: Pat.Pat GHC.GhcTc -> Bool
                                        f (Pat.VarPat _ _) = True
                                        f _ = False
                                    let vs::[Pat.Pat GHC.GhcTc] = SYB.listify f pat
                                    let name::String = concatMap (GHC.showPpr GHC.unsafeGlobalDynFlags) vs
                                    let bodyLoc = GHC.getLoc body
                                    let body0a::GHC.LHsExpr GHC.GhcTc = GHC.L bodyLoc $ GHC.HsWrap GHC.noExt (GHC.WpTyApp tyArg) $ GHC.HsVar GHC.noExt (GHC.L (GHC.getLoc body) namer)
                                    let body0b::GHC.LHsExpr GHC.GhcTc = GHC.L bodyLoc $ GHC.HsLit GHC.noExt (GHC.HsString GHC.NoSourceText (GHC.fsLit name))
                                    let body1::GHC.LHsExpr GHC.GhcTc = GHC.L bodyLoc $ GHC.HsApp GHC.noExt body0a body0b
                                    let body2::GHC.LHsExpr GHC.GhcTc = GHC.L bodyLoc $ GHC.HsPar GHC.noExt body
                                    let body3::GHC.LHsExpr GHC.GhcTc = GHC.L bodyLoc $ GHC.HsApp GHC.noExt body1 body2
                                    debug $ GHC.showPpr GHC.unsafeGlobalDynFlags body3

                                    debug $ GHC.showPpr GHC.unsafeGlobalDynFlags e
                                    debug $ GHC.showPpr GHC.unsafeGlobalDynFlags pat
                                    debug name
                                    debug $ GHC.showPpr GHC.unsafeGlobalDynFlags body
                                    debug $ GHC.showPpr GHC.unsafeGlobalDynFlags expr0
                                    debug $ GHC.showPpr GHC.unsafeGlobalDynFlags expr1
                                    debug $ GHC.showPpr GHC.unsafeGlobalDynFlags $ GHC.L loc (Expr.BindStmt xbind pat (body3) expr0 expr1)
                                    debug "--------------------------------------------------"
                                    return $ GHC.L loc (Expr.BindStmt xbind pat body3 expr0 expr1)
                            else do debug $ "======================================> WEIRD"
                                    debug $ GHC.showPpr GHC.unsafeGlobalDynFlags tyC
                                    debug $ GHC.showPpr GHC.unsafeGlobalDynFlags (GHC.occName $ GHC.tyConName tyC)
                                    return e
                        _ -> do debug "===============================> SO SAD"
                                return e
    _ -> return e

showBinds e = return e


install :: [GHC.CommandLineOption] -> GHC.ModSummary -> GHC.TcGblEnv -> GHC.TcM GHC.TcGblEnv
install opts modS tcGblEnv = do
  tcg_binds <-
    SYB.mkM showBinds `SYB.everywhereM` GHC.tcg_binds tcGblEnv
  return $ tcGblEnv { GHC.tcg_binds = tcg_binds }
