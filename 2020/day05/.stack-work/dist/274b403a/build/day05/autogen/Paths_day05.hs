{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_day05 (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\caiod\\Documents\\fayner\\haskell\\advent_of_code\\Seno\\day05\\.stack-work\\install\\48fd5fe8\\bin"
libdir     = "C:\\Users\\caiod\\Documents\\fayner\\haskell\\advent_of_code\\Seno\\day05\\.stack-work\\install\\48fd5fe8\\lib\\x86_64-windows-ghc-8.10.3\\day05-0.0.0-LppxrgPPj6WK4K3jILJNfn-day05"
dynlibdir  = "C:\\Users\\caiod\\Documents\\fayner\\haskell\\advent_of_code\\Seno\\day05\\.stack-work\\install\\48fd5fe8\\lib\\x86_64-windows-ghc-8.10.3"
datadir    = "C:\\Users\\caiod\\Documents\\fayner\\haskell\\advent_of_code\\Seno\\day05\\.stack-work\\install\\48fd5fe8\\share\\x86_64-windows-ghc-8.10.3\\day05-0.0.0"
libexecdir = "C:\\Users\\caiod\\Documents\\fayner\\haskell\\advent_of_code\\Seno\\day05\\.stack-work\\install\\48fd5fe8\\libexec\\x86_64-windows-ghc-8.10.3\\day05-0.0.0"
sysconfdir = "C:\\Users\\caiod\\Documents\\fayner\\haskell\\advent_of_code\\Seno\\day05\\.stack-work\\install\\48fd5fe8\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "day05_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "day05_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "day05_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "day05_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "day05_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "day05_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
