{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_regex_base (
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
version = Version [0,94,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/anjan/.cabal/store/ghc-8.6.5/regex-base-0.94.0.0-83cf4d8eac97e9aa145a3d27ac67bed98c4e85b4079681c563b9474b72d6c8c0/bin"
libdir     = "/home/anjan/.cabal/store/ghc-8.6.5/regex-base-0.94.0.0-83cf4d8eac97e9aa145a3d27ac67bed98c4e85b4079681c563b9474b72d6c8c0/lib"
dynlibdir  = "/home/anjan/.cabal/store/ghc-8.6.5/regex-base-0.94.0.0-83cf4d8eac97e9aa145a3d27ac67bed98c4e85b4079681c563b9474b72d6c8c0/lib"
datadir    = "/home/anjan/.cabal/store/ghc-8.6.5/regex-base-0.94.0.0-83cf4d8eac97e9aa145a3d27ac67bed98c4e85b4079681c563b9474b72d6c8c0/share"
libexecdir = "/home/anjan/.cabal/store/ghc-8.6.5/regex-base-0.94.0.0-83cf4d8eac97e9aa145a3d27ac67bed98c4e85b4079681c563b9474b72d6c8c0/libexec"
sysconfdir = "/home/anjan/.cabal/store/ghc-8.6.5/regex-base-0.94.0.0-83cf4d8eac97e9aa145a3d27ac67bed98c4e85b4079681c563b9474b72d6c8c0/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "regex_base_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "regex_base_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "regex_base_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "regex_base_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "regex_base_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "regex_base_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
