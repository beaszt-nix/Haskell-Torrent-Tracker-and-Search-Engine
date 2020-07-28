{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_bencode (
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
version = Version [0,6,1,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/anjan/.cabal/store/ghc-8.6.5/bencode-0.6.1.1-84776fb4528b7438362374c6b51013d6ebbe1bb26303481231f191172918d73d/bin"
libdir     = "/home/anjan/.cabal/store/ghc-8.6.5/bencode-0.6.1.1-84776fb4528b7438362374c6b51013d6ebbe1bb26303481231f191172918d73d/lib"
dynlibdir  = "/home/anjan/.cabal/store/ghc-8.6.5/bencode-0.6.1.1-84776fb4528b7438362374c6b51013d6ebbe1bb26303481231f191172918d73d/lib"
datadir    = "/home/anjan/.cabal/store/ghc-8.6.5/bencode-0.6.1.1-84776fb4528b7438362374c6b51013d6ebbe1bb26303481231f191172918d73d/share"
libexecdir = "/home/anjan/.cabal/store/ghc-8.6.5/bencode-0.6.1.1-84776fb4528b7438362374c6b51013d6ebbe1bb26303481231f191172918d73d/libexec"
sysconfdir = "/home/anjan/.cabal/store/ghc-8.6.5/bencode-0.6.1.1-84776fb4528b7438362374c6b51013d6ebbe1bb26303481231f191172918d73d/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bencode_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bencode_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "bencode_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "bencode_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bencode_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bencode_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
