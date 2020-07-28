{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_saltine (
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
version = Version [0,1,1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/anjan/.cabal/store/ghc-8.6.5/saltine-0.1.1.0-58430be2299ecee29b7f041cbd74caf71d6e2587a79957fd70ed7cf1f4400504/bin"
libdir     = "/home/anjan/.cabal/store/ghc-8.6.5/saltine-0.1.1.0-58430be2299ecee29b7f041cbd74caf71d6e2587a79957fd70ed7cf1f4400504/lib"
dynlibdir  = "/home/anjan/.cabal/store/ghc-8.6.5/saltine-0.1.1.0-58430be2299ecee29b7f041cbd74caf71d6e2587a79957fd70ed7cf1f4400504/lib"
datadir    = "/home/anjan/.cabal/store/ghc-8.6.5/saltine-0.1.1.0-58430be2299ecee29b7f041cbd74caf71d6e2587a79957fd70ed7cf1f4400504/share"
libexecdir = "/home/anjan/.cabal/store/ghc-8.6.5/saltine-0.1.1.0-58430be2299ecee29b7f041cbd74caf71d6e2587a79957fd70ed7cf1f4400504/libexec"
sysconfdir = "/home/anjan/.cabal/store/ghc-8.6.5/saltine-0.1.1.0-58430be2299ecee29b7f041cbd74caf71d6e2587a79957fd70ed7cf1f4400504/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "saltine_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "saltine_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "saltine_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "saltine_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "saltine_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "saltine_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
