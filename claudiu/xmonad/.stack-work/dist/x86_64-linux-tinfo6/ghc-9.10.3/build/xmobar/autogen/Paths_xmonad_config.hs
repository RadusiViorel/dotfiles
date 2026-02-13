{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_xmonad_config (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,1,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/void/.config/xmonad/.stack-work/install/x86_64-linux-tinfo6/c154ae0f9363d8552c8d5e9256ee46017e57ef136cf4d63b925fbdb73663bc0e/9.10.3/bin"
libdir     = "/home/void/.config/xmonad/.stack-work/install/x86_64-linux-tinfo6/c154ae0f9363d8552c8d5e9256ee46017e57ef136cf4d63b925fbdb73663bc0e/9.10.3/lib/x86_64-linux-ghc-9.10.3-56e0/xmonad-config-0.1.0-DamJ7nQa2w68g48GxMCsNi-xmobar"
dynlibdir  = "/home/void/.config/xmonad/.stack-work/install/x86_64-linux-tinfo6/c154ae0f9363d8552c8d5e9256ee46017e57ef136cf4d63b925fbdb73663bc0e/9.10.3/lib/x86_64-linux-ghc-9.10.3-56e0"
datadir    = "/home/void/.config/xmonad/.stack-work/install/x86_64-linux-tinfo6/c154ae0f9363d8552c8d5e9256ee46017e57ef136cf4d63b925fbdb73663bc0e/9.10.3/share/x86_64-linux-ghc-9.10.3-56e0/xmonad-config-0.1.0"
libexecdir = "/home/void/.config/xmonad/.stack-work/install/x86_64-linux-tinfo6/c154ae0f9363d8552c8d5e9256ee46017e57ef136cf4d63b925fbdb73663bc0e/9.10.3/libexec/x86_64-linux-ghc-9.10.3-56e0/xmonad-config-0.1.0"
sysconfdir = "/home/void/.config/xmonad/.stack-work/install/x86_64-linux-tinfo6/c154ae0f9363d8552c8d5e9256ee46017e57ef136cf4d63b925fbdb73663bc0e/9.10.3/etc"

getBinDir     = catchIO (getEnv "xmonad_config_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "xmonad_config_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "xmonad_config_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "xmonad_config_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xmonad_config_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "xmonad_config_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
