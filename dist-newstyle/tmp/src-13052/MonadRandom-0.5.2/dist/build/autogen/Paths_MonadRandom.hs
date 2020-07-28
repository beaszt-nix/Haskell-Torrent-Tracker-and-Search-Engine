{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_MonadRandom (
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
version = Version [0,5,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/anjan/.cabal/store/ghc-8.6.5/MonadRandom-0.5.2-8db9f84ad09373c177c27c2d3235fc3b33ab82e66a8d6e23a57d010a664dcd50/bin"
libdir     = "/home/anjan/.cabal/store/ghc-8.6.5/MonadRandom-0.5.2-8db9f84ad09373c177c27c2d3235fc3b33ab82e66a8d6e23a57d010a664dcd50/lib"
dynlibdir  = "/home/anjan/.cabal/store/ghc-8.6.5/MonadRandom-0.5.2-8db9f84ad09373c177c27c2d3235fc3b33ab82e66a8d6e23a57d010a664dcd50/lib"
datadir    = "/home/anjan/.cabal/store/ghc-8.6.5/MonadRandom-0.5.2-8db9f84ad09373c177c27c2d3235fc3b33ab82e66a8d6e23a57d010a664dcd50/share"
libexecdir = "/home/anjan/.cabal/store/ghc-8.6.5/MonadRandom-0.5.2-8db9f84ad09373c177c27c2d3235fc3b33ab82e66a8d6e23a57d010a664dcd50/libexec"
sysconfdir = "/home/anjan/.cabal/store/ghc-8.6.5/MonadRandom-0.5.2-8db9f84ad09373c177c27c2d3235fc3b33ab82e66a8d6e23a57d010a664dcd50/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "MonadRandom_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "MonadRandom_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "MonadRandom_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "MonadRandom_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "MonadRandom_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "MonadRandom_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
