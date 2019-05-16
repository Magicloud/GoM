{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as C8
import           Data.Char
import           GoM.Types
import           NotifySend
import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           System.Posix.Signals
import           System.Posix.Types
import           System.Posix.User

main :: IO ()
main = do
  let limit0s = 393216
      limit0h = 524288
  me <- getRealUserID
  watch me limit0s limit0h

watch :: UserID -> Integer -> Integer -> IO ()
watch u limit0s limit0h = do
  listDirectory "/proc/" >>= mapM_ (\fp -> do
    isMyPid' <- maybe False id <$> wrap2Maybe (isMyPid fp u)
    when isMyPid' $ do
      mStat <- wrap2Maybe (C8.readFile ("/proc/" </> fp </> "stat")) >>=
        return . flip (>>=) readC8
      case mStat of
        Just stat -> do
          let rss = statRss stat
          if rss >= limit0s
            then if rss >= limit0h
              then kill stat
              else alert stat
            else
              return ()
        Nothing -> return ())
  threadDelay 1000000
  watch u limit0s limit0h
  where
    wrap2Maybe :: IO a -> IO (Maybe a)
    wrap2Maybe f = catch ((<$>) Just $! f) (\(_ :: IOException) -> return Nothing)
    isMyPid :: FilePath -> UserID -> IO Bool
    isMyPid fp me = do
      let areDigit = all isDigit fp
      isDir <- doesDirectoryExist $ "/proc/" </> fp
      owner <- fileOwner <$> getFileStatus ("/proc" </> fp)
      return $ areDigit && isDir && (owner == me)
    alert :: Stat -> IO ()
    alert s = do
      void $ notify "GoM" Nothing "" (show (statPid s) ++ "|" ++ take 10 (statComm s) ++ " is reaching soft limit") "" 100000
      putStrLn $ statComm s
    kill :: Stat -> IO ()
    kill s = do
      void $ notify "GoM" Nothing "" (show (statPid s) ++ "|" ++ take 10 (statComm s) ++ " is reaching hard limit") "" 100000
      putStrLn $ statComm s
      signalProcess sigKILL $ statPid s
