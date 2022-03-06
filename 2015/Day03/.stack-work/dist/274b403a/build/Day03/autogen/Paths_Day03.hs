{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Day03 (
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
version = Version [0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\caiod\\Documents\\Fayner\\haskell\\Seno_estudando\\aoc2015\\Day03\\.stack-work\\install\\93b9ca3b\\bin"
libdir     = "C:\\Users\\caiod\\Documents\\Fayner\\haskell\\Seno_estudando\\aoc2015\\Day03\\.stack-work\\install\\93b9ca3b\\lib\\x86_64-windows-ghc-8.10.4\\Day03-0.1-EkQoOTMyEGbFkSmMJA814-Day03"
dynlibdir  = "C:\\Users\\caiod\\Documents\\Fayner\\haskell\\Seno_estudando\\aoc2015\\Day03\\.stack-work\\install\\93b9ca3b\\lib\\x86_64-windows-ghc-8.10.4"
datadir    = "C:\\Users\\caiod\\Documents\\Fayner\\haskell\\Seno_estudando\\aoc2015\\Day03\\.stack-work\\install\\93b9ca3b\\share\\x86_64-windows-ghc-8.10.4\\Day03-0.1"
libexecdir = "C:\\Users\\caiod\\Documents\\Fayner\\haskell\\Seno_estudando\\aoc2015\\Day03\\.stack-work\\install\\93b9ca3b\\libexec\\x86_64-windows-ghc-8.10.4\\Day03-0.1"
sysconfdir = "C:\\Users\\caiod\\Documents\\Fayner\\haskell\\Seno_estudando\\aoc2015\\Day03\\.stack-work\\install\\93b9ca3b\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Day03_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Day03_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Day03_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Day03_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Day03_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Day03_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
