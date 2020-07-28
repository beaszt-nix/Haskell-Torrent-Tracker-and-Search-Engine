{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cpu (
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
version = Version [0,1,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/anjan/.cabal/store/ghc-8.6.5/cpu-0.1.2-6f1c3da68f455d53ae557e709e2f586e9762801e72ece21d44a77a7b1533773e/bin"
libdir     = "/home/anjan/.cabal/store/ghc-8.6.5/cpu-0.1.2-6f1c3da68f455d53ae557e709e2f586e9762801e72ece21d44a77a7b1533773e/lib"
dynlibdir  = "/home/anjan/.cabal/store/ghc-8.6.5/cpu-0.1.2-6f1c3da68f455d53ae557e709e2f586e9762801e72ece21d44a77a7b1533773e/lib"
datadir    = "/home/anjan/.cabal/store/ghc-8.6.5/cpu-0.1.2-6f1c3da68f455d53ae557e709e2f586e9762801e72ece21d44a77a7b1533773e/share"
libexecdir = "/home/anjan/.cabal/store/ghc-8.6.5/cpu-0.1.2-6f1c3da68f455d53ae557e709e2f586e9762801e72ece21d44a77a7b1533773e/libexec"
sysconfdir = "/home/anjan/.cabal/store/ghc-8.6.5/cpu-0.1.2-6f1c3da68f455d53ae557e709e2f586e9762801e72ece21d44a77a7b1533773e/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cpu_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cpu_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cpu_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cpu_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cpu_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cpu_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
