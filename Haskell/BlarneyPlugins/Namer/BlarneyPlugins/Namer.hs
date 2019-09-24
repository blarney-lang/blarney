{-|
Module      : Namer
Description : Namer plugin for Blarney
Copyright   : (c) Alexandre Joannou, 2019
                  Matthew Naylor, 2019
License     : MIT
Maintainer  : alexandre.joannou@gmail.com
Stability   : experimental

This module defines a Namer ghc plugin for Blarney to preserve names from
Blarney source code down to generated code.

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
import qualified Data.Generics as SYB
import Data.IORef
import Control.Monad

-- | Printing helpers
msg :: String -> GHC.TcM ()
msg = liftIO . putStrLn
ppr :: GHC.Outputable a => a -> String
ppr = GHC.showPpr GHC.unsafeGlobalDynFlags
pprMsg :: GHC.Outputable a => a -> GHC.TcM ()
pprMsg = msg . ppr

-- | The exported 'GHC.Plugin'. Defines a custom type checker pass.
plugin :: GHC.Plugin
plugin = GHC.defaultPlugin {
           GHC.typeCheckResultAction = tcPass
         , GHC.pluginRecompile = \_ -> return GHC.NoForceRecompile
         }

-- | The type checker pass.
tcPass :: [GHC.CommandLineOption] -> GHC.ModSummary -> GHC.TcGblEnv
       -> GHC.TcM GHC.TcGblEnv
tcPass _ modS env = do
  count  <- liftIO $ newIORef 0
  hs_env <- GHC.getTopEnv
  blMod  <- liftIO $ GHC.findImportedModule hs_env
                    (GHC.mkModuleName "Blarney.Module") Nothing
  tcg_binds <- SYB.mkM (nameModule count blMod)
               `SYB.everywhereM` GHC.tcg_binds env
  n <- liftIO $ readIORef count
  when (n > 0) $
    msg $ "\tBlarney's Namer pluging preserved " ++ show n ++ " module name"
                                                 ++ if n > 1 then "s" else ""
  --msg $ show (GHC.ms_location modS)
  return $ env { GHC.tcg_binds = tcg_binds }

-- | Helper function to preserve Blarney modules' instance name.
nameModule :: IORef Int ->  GHC.FindResult -> Expr.ExprLStmt GHC.GhcTc
           -> GHC.TcM (Expr.ExprLStmt GHC.GhcTc)
nameModule count (GHC.Found _ m) e@(GHC.L loc (Expr.BindStmt xbind pat body e0 e1)) = do
  hs_env <- GHC.getTopEnv
  blModuleTy <- GHC.lookupOrig m (GHC.mkTcOcc "Module")
  (_, mbe) <- liftIO (GHC.deSugarExpr hs_env body)
  case CoreUtils.exprType <$> mbe of
    Nothing -> return e
    Just t  -> case Type.splitTyConApp_maybe t of
      Just (tyC, [tyArg]) | GHC.tyConName tyC == blModuleTy -> do
        namer <- GHC.lookupId =<<
                 GHC.lookupOrig m (GHC.mkVarOcc "withNewName")
        let isVarPat :: Pat.Pat GHC.GhcTc -> Bool
            isVarPat (Pat.VarPat _ _) = True
            isVarPat _ = False
        let vs     = SYB.listify isVarPat pat
        let name   = concatMap (GHC.showPpr GHC.unsafeGlobalDynFlags) vs
        let bLoc   = GHC.L $ GHC.getLoc body
        let namerE = bLoc $ GHC.HsWrap GHC.noExt (GHC.WpTyApp tyArg)
                          $ GHC.HsVar GHC.noExt (bLoc namer)
        let nameE  = bLoc $ GHC.HsLit GHC.noExt
                          $ GHC.HsString GHC.NoSourceText (GHC.fsLit name)
        let namedE = bLoc $ GHC.HsApp GHC.noExt namerE nameE
        let body'  = bLoc $ GHC.HsApp GHC.noExt namedE body
        liftIO $ modifyIORef count (+1)
        return $ GHC.L loc (Expr.BindStmt xbind pat body' e0 e1)
      _ -> return e
nameModule _ _ e = return e
