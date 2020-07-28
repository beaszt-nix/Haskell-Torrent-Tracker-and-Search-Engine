{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_readable (
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
version = Version [0,3,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/anjan/.cabal/store/ghc-8.6.5/readable-0.3.1-74d7520615a73e9d151321fcfa5ddf02f9d7ede17dceecd98c1d54a894a76bbb/bin"
libdir     = "/home/anjan/.cabal/store/ghc-8.6.5/readable-0.3.1-74d7520615a73e9d151321fcfa5ddf02f9d7ede17dceecd98c1d54a894a76bbb/lib"
dynlibdir  = "/home/anjan/.cabal/store/ghc-8.6.5/readable-0.3.1-74d7520615a73e9d151321fcfa5ddf02f9d7ede17dceecd98c1d54a894a76bbb/lib"
datadir    = "/home/anjan/.cabal/store/ghc-8.6.5/readable-0.3.1-74d7520615a73e9d151321fcfa5ddf02f9d7ede17dceecd98c1d54a894a76bbb/share"
libexecdir = "/home/anjan/.cabal/store/ghc-8.6.5/readable-0.3.1-74d7520615a73e9d151321fcfa5ddf02f9d7ede17dceecd98c1d54a894a76bbb/libexec"
sysconfdir = "/home/anjan/.cabal/store/ghc-8.6.5/readable-0.3.1-74d7520615a73e9d151321fcfa5ddf02f9d7ede17dceecd98c1d54a894a76bbb/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "readable_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "readable_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "readable_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "readable_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "readable_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "readable_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
