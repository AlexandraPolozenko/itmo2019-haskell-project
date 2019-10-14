{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_battlefield (
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

bindir     = "/Users/aleksandra/Desktop/haskell/itmo2019-haskell-project/.stack-work/install/x86_64-osx/lts-14.6/8.6.5/bin"
libdir     = "/Users/aleksandra/Desktop/haskell/itmo2019-haskell-project/.stack-work/install/x86_64-osx/lts-14.6/8.6.5/lib/x86_64-osx-ghc-8.6.5/battlefield-0.1.0.0-8CmZqnTevv4DQlKkYwSBkE"
dynlibdir  = "/Users/aleksandra/Desktop/haskell/itmo2019-haskell-project/.stack-work/install/x86_64-osx/lts-14.6/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/aleksandra/Desktop/haskell/itmo2019-haskell-project/.stack-work/install/x86_64-osx/lts-14.6/8.6.5/share/x86_64-osx-ghc-8.6.5/battlefield-0.1.0.0"
libexecdir = "/Users/aleksandra/Desktop/haskell/itmo2019-haskell-project/.stack-work/install/x86_64-osx/lts-14.6/8.6.5/libexec/x86_64-osx-ghc-8.6.5/battlefield-0.1.0.0"
sysconfdir = "/Users/aleksandra/Desktop/haskell/itmo2019-haskell-project/.stack-work/install/x86_64-osx/lts-14.6/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "battlefield_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "battlefield_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "battlefield_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "battlefield_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "battlefield_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "battlefield_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
