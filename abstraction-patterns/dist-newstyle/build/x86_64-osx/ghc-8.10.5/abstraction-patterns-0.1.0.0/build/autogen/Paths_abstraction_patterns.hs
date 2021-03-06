{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_abstraction_patterns (
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

bindir     = "/Users/bradleyheather/.cabal/bin"
libdir     = "/Users/bradleyheather/.cabal/lib/x86_64-osx-ghc-8.10.5/abstraction-patterns-0.1.0.0-inplace"
dynlibdir  = "/Users/bradleyheather/.cabal/lib/x86_64-osx-ghc-8.10.5"
datadir    = "/Users/bradleyheather/.cabal/share/x86_64-osx-ghc-8.10.5/abstraction-patterns-0.1.0.0"
libexecdir = "/Users/bradleyheather/.cabal/libexec/x86_64-osx-ghc-8.10.5/abstraction-patterns-0.1.0.0"
sysconfdir = "/Users/bradleyheather/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "abstraction_patterns_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "abstraction_patterns_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "abstraction_patterns_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "abstraction_patterns_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "abstraction_patterns_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "abstraction_patterns_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
