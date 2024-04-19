{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tui (
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
version = Version [0,0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/lucas/\193rea de Trabalho/Programa\231\227o Funcional/projeto_final/tui-base/.stack-work/install/x86_64-linux-tinfo6/d100edc57c65c9687ec3dd2005419bfc6b094ad662d0fc29213a3e3aea9ebbd1/8.4.4/bin"
libdir     = "/home/lucas/\193rea de Trabalho/Programa\231\227o Funcional/projeto_final/tui-base/.stack-work/install/x86_64-linux-tinfo6/d100edc57c65c9687ec3dd2005419bfc6b094ad662d0fc29213a3e3aea9ebbd1/8.4.4/lib/x86_64-linux-ghc-8.4.4/tui-0.0.0.0-EoEup05weNJHz27BvW5WDF-sokoban-tui"
dynlibdir  = "/home/lucas/\193rea de Trabalho/Programa\231\227o Funcional/projeto_final/tui-base/.stack-work/install/x86_64-linux-tinfo6/d100edc57c65c9687ec3dd2005419bfc6b094ad662d0fc29213a3e3aea9ebbd1/8.4.4/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/home/lucas/\193rea de Trabalho/Programa\231\227o Funcional/projeto_final/tui-base/.stack-work/install/x86_64-linux-tinfo6/d100edc57c65c9687ec3dd2005419bfc6b094ad662d0fc29213a3e3aea9ebbd1/8.4.4/share/x86_64-linux-ghc-8.4.4/tui-0.0.0.0"
libexecdir = "/home/lucas/\193rea de Trabalho/Programa\231\227o Funcional/projeto_final/tui-base/.stack-work/install/x86_64-linux-tinfo6/d100edc57c65c9687ec3dd2005419bfc6b094ad662d0fc29213a3e3aea9ebbd1/8.4.4/libexec/x86_64-linux-ghc-8.4.4/tui-0.0.0.0"
sysconfdir = "/home/lucas/\193rea de Trabalho/Programa\231\227o Funcional/projeto_final/tui-base/.stack-work/install/x86_64-linux-tinfo6/d100edc57c65c9687ec3dd2005419bfc6b094ad662d0fc29213a3e3aea9ebbd1/8.4.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tui_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tui_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tui_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tui_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tui_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tui_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
