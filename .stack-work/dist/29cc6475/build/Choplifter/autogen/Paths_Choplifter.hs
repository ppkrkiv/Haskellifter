{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Choplifter (
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

bindir     = "C:\\Users\\pyryk\\Documents\\GitHub\\Haskellifter\\.stack-work\\install\\b3352992\\bin"
libdir     = "C:\\Users\\pyryk\\Documents\\GitHub\\Haskellifter\\.stack-work\\install\\b3352992\\lib\\x86_64-windows-ghc-8.8.4\\Choplifter-0.1.0.0-D5rY6i2IJgC1RRHY3Mepd-Choplifter"
dynlibdir  = "C:\\Users\\pyryk\\Documents\\GitHub\\Haskellifter\\.stack-work\\install\\b3352992\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "C:\\Users\\pyryk\\Documents\\GitHub\\Haskellifter\\.stack-work\\install\\b3352992\\share\\x86_64-windows-ghc-8.8.4\\Choplifter-0.1.0.0"
libexecdir = "C:\\Users\\pyryk\\Documents\\GitHub\\Haskellifter\\.stack-work\\install\\b3352992\\libexec\\x86_64-windows-ghc-8.8.4\\Choplifter-0.1.0.0"
sysconfdir = "C:\\Users\\pyryk\\Documents\\GitHub\\Haskellifter\\.stack-work\\install\\b3352992\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Choplifter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Choplifter_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Choplifter_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Choplifter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Choplifter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Choplifter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
