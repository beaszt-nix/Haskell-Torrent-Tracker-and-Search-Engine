{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_summer_proj (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/anjan/.cabal/bin"
libdir     = "/home/anjan/.cabal/lib/x86_64-linux-ghc-8.6.5/summer-proj-0.1.0.0-3OdkbGDa1oWDxkva6dKpie"
dynlibdir  = "/home/anjan/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/anjan/.cabal/share/x86_64-linux-ghc-8.6.5/summer-proj-0.1.0.0"
libexecdir = "/home/anjan/.cabal/libexec/x86_64-linux-ghc-8.6.5/summer-proj-0.1.0.0"
sysconfdir = "/home/anjan/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "summer_proj_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "summer_proj_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "summer_proj_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "summer_proj_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "summer_proj_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "summer_proj_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
