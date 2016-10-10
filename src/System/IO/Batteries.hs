{-# LANGUAGE CPP #-}

module System.IO.Batteries
  ( nativeFileSeparator, mkPath
  , ePutStr, ePutStrLn
  , putErrMsg, putWarnS, putErrS
  ) where

import Data.String.ANSI

import Data.List
import System.Environment
import System.IO

nativeFileSeparator :: Char
#ifdef mingw32_HOST_OS
nativeFileSeparator = '\\'
#else
nativeFileSeparator = '/'
#endif

mkPath :: [String] -> FilePath
mkPath = intercalate [nativeFileSeparator]

ePutStr :: String -> IO ()
ePutStr = hPutStr stderr

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

putWarnS :: String -> IO ()
putWarnS = putErrMsg (toYellow "warning")

putErrS :: String -> IO ()
putErrS = putErrMsg (toRed "error")

putErrMsg :: String -> String -> IO ()
putErrMsg header errMsg = do
  progName <- getProgName
  ePutStrLn $ header ++ " (" ++ progName ++ "): " ++ errMsg
