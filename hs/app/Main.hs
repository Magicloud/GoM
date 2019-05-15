{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.Maybe
import GoM.Types
import NotifySend
import System.Directory
import System.FilePath
import System.Posix.Files
import System.Posix.Signals
import System.Posix.Types
import System.Posix.User

main :: IO ()
main = do
  -- default 1st derivative limit (soft, hard)
  let limit0s = 1610612736
      limit0h = 2147483648
  me <- getRealUserID
  watch me limit0s limit0h

watch :: UserID -> Integer -> Integer -> IO ()
watch u limit0s limit0h = do
  listDirectory "/proc/" >>= mapM_ (\fp -> do
    isMyPid' <- isMyPid fp u
    if isMyPid'
      then do
        stat <- fromJust . parseStat <$> readFile ("/proc/" </> fp </> "stat")
        let rss = statRss stat
        if rss >= limit0s
          then if rss >= limit0h
            then kill stat
            else alert stat
          else
            return ()
      else return ())
  threadDelay 1000000
  watch u limit0s limit0h
  where
    isMyPid :: FilePath -> UserID -> IO Bool
    isMyPid fp me = catch (do
      let areDigit = all isDigit fp
      isDir <- doesDirectoryExist $ "/proc/" </> fp
      owner <- fileOwner <$> getFileStatus fp
      return $ areDigit && isDir && (owner == me)) $ \(_ :: IOException) -> return False
    alert :: Stat -> IO ()
    alert s = void $ notify "GoM" Nothing "" (show (statPid s) ++ "|" ++ take 10 (statComm s) ++ " is reaching soft limit") "" 10000
    kill :: Stat -> IO ()
    kill s = do
      void $ notify "GoM" Nothing "" (show (statPid s) ++ "|" ++ take 10 (statComm s) ++ " is reaching soft limit") "" 10000
      signalProcess sigKILL $ statPid s
