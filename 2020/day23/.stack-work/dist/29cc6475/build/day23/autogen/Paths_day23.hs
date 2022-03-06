{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_day23 (
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

bindir     = "C:\\Users\\caiod\\Documents\\fayner\\haskell\\advent_of_code\\Seno\\day23\\.stack-work\\install\\02628c58\\bin"
libdir     = "C:\\Users\\caiod\\Documents\\fayner\\haskell\\advent_of_code\\Seno\\day23\\.stack-work\\install\\02628c58\\lib\\x86_64-windows-ghc-8.8.4\\day23-0.1.0.0-KKAUDG7WdIgKL5s61aLegK-day23"
dynlibdir  = "C:\\Users\\caiod\\Documents\\fayner\\haskell\\advent_of_code\\Seno\\day23\\.stack-work\\install\\02628c58\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "C:\\Users\\caiod\\Documents\\fayner\\haskell\\advent_of_code\\Seno\\day23\\.stack-work\\install\\02628c58\\share\\x86_64-windows-ghc-8.8.4\\day23-0.1.0.0"
libexecdir = "C:\\Users\\caiod\\Documents\\fayner\\haskell\\advent_of_code\\Seno\\day23\\.stack-work\\install\\02628c58\\libexec\\x86_64-windows-ghc-8.8.4\\day23-0.1.0.0"
sysconfdir = "C:\\Users\\caiod\\Documents\\fayner\\haskell\\advent_of_code\\Seno\\day23\\.stack-work\\install\\02628c58\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "day23_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "day23_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "day23_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "day23_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "day23_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "day23_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
