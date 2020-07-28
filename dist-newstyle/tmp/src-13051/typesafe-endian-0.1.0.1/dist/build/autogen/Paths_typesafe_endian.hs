{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_typesafe_endian (
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
version = Version [0,1,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/anjan/.cabal/store/ghc-8.6.5/typesafe-endian-0.1.0.1-b92cb3b84135ef8ee66d76fb3925ebb2364055bc744490fa56bc8f7f9f300ef0/bin"
libdir     = "/home/anjan/.cabal/store/ghc-8.6.5/typesafe-endian-0.1.0.1-b92cb3b84135ef8ee66d76fb3925ebb2364055bc744490fa56bc8f7f9f300ef0/lib"
dynlibdir  = "/home/anjan/.cabal/store/ghc-8.6.5/typesafe-endian-0.1.0.1-b92cb3b84135ef8ee66d76fb3925ebb2364055bc744490fa56bc8f7f9f300ef0/lib"
datadir    = "/home/anjan/.cabal/store/ghc-8.6.5/typesafe-endian-0.1.0.1-b92cb3b84135ef8ee66d76fb3925ebb2364055bc744490fa56bc8f7f9f300ef0/share"
libexecdir = "/home/anjan/.cabal/store/ghc-8.6.5/typesafe-endian-0.1.0.1-b92cb3b84135ef8ee66d76fb3925ebb2364055bc744490fa56bc8f7f9f300ef0/libexec"
sysconfdir = "/home/anjan/.cabal/store/ghc-8.6.5/typesafe-endian-0.1.0.1-b92cb3b84135ef8ee66d76fb3925ebb2364055bc744490fa56bc8f7f9f300ef0/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "typesafe_endian_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "typesafe_endian_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "typesafe_endian_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "typesafe_endian_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "typesafe_endian_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "typesafe_endian_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
