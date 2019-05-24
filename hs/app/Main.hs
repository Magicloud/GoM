{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as C8
import           Data.Char
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set.Ordered as OS
#ifdef DEBUG
import           Debug.Trace
#else
import           Debug.NoTrace
#endif
import           GoM.Types
import           NotifySend
import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           System.Posix.Signals
import           System.Posix.SysConf
import           System.Posix.Types
import           System.Posix.User

data SHN = Soft
         | Hard

type HandledDict = M.Map (ProcessID, String) SHN

main :: IO ()
main = do
  Right pagesize <- sysconf ScPagesize
  let limit0s = (1024 + 512) * 1024 * 1024 `div` pagesize
      limit0h = 2 * 1024 * 1024 * 1024 `div` pagesize
  me <- getRealUserID
  void $ runStateT (watch me limit0s limit0h) M.empty

watch :: UserID -> Integer -> Integer -> StateT HandledDict IO ()
watch u limit0s limit0h = do
  handledDict <- get
  liftIO (listDirectory "/proc/" >>=
    (fmap traceShowId . filterM (\fp -> maybe False id <$> wrap2Maybe (isMyPid fp u))) >>=
      foldM (\hd fp -> wrap2Maybe (C8.readFile ("/proc/" </> fp </> "stat")) >>=
        return . flip (>>=) readC8 >>= \case
          Just stat ->
            let k = (statPid stat, statComm stat)
                handled = hd M.!? k
            in case (statRss stat `inRange` OS.fromList [0, limit0s, limit0h], handled) of
              (InRange 0, Just _) -> return $ M.delete k hd
              (InRange 1, Nothing) -> alert stat >> return (M.insert k Soft hd)
              (InRange 1, Just Hard) -> return $ M.update (const $ Just Soft) k hd
              (Bigger, Nothing) -> kill stat >> return (M.insert k Hard hd)
              (Bigger, Just Soft) -> kill stat >> return (M.update (const $ Just Hard) k hd)
              _ -> return hd
          Nothing -> traceShow fp $ return hd) handledDict) >>=
    put . traceShowFn M.size
  liftIO $ threadDelay $ minute 1
  watch u limit0s limit0h
  where
    minute :: Int -> Int
    minute = (*) 60000000
    traceShowFn :: (Show b) => (a -> b) -> a -> a
    traceShowFn f a = traceShow (f a) a
    wrap2Maybe :: IO a -> IO (Maybe a)
    wrap2Maybe f = catch ((<$>) Just $! f) (\(e :: IOException) -> traceShow e $ return Nothing)
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
      signalProcess sigTERM $ statPid s

data InRange = Smaller
             | InRange Int
             | Bigger

inRange :: (Ord a) => a -> OS.OSet a -> InRange
inRange x xs
  | x < fromJust (xs `OS.elemAt` 0) = Smaller
  | x >= fromJust (xs `OS.elemAt` (OS.size xs - 1)) = Bigger
  | otherwise =
    let (i, _, _) = head $ filter (\(_, l, u) -> x >= l && x < u) $ zip3 [0..] (OS.toAscList xs) (tail $ OS.toAscList xs)
    in InRange i
