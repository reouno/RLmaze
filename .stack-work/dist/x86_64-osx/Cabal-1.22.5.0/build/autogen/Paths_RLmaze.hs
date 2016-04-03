module Paths_RLmaze (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/reo/hsproject/RLmaze/.stack-work/install/x86_64-osx/lts-5.10/7.10.3/bin"
libdir     = "/Users/reo/hsproject/RLmaze/.stack-work/install/x86_64-osx/lts-5.10/7.10.3/lib/x86_64-osx-ghc-7.10.3/RLmaze-0.1.0.0-6v2m9a4QblzKGxRWi4tEqe"
datadir    = "/Users/reo/hsproject/RLmaze/.stack-work/install/x86_64-osx/lts-5.10/7.10.3/share/x86_64-osx-ghc-7.10.3/RLmaze-0.1.0.0"
libexecdir = "/Users/reo/hsproject/RLmaze/.stack-work/install/x86_64-osx/lts-5.10/7.10.3/libexec"
sysconfdir = "/Users/reo/hsproject/RLmaze/.stack-work/install/x86_64-osx/lts-5.10/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "RLmaze_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "RLmaze_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "RLmaze_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "RLmaze_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "RLmaze_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
