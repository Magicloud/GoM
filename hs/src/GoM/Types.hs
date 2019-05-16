{-# LANGUAGE TypeSynonymInstances #-}
module GoM.Types where

import           Data.Bits
import qualified Data.ByteString.Char8 as C8
import           Data.Word
import           GoM.Helper
import           System.Exit
import           System.Posix.Signals
import           System.Posix.Types

class ReadC8 a where
  readC8 :: C8.ByteString -> Maybe a

data ProcState = PSRunning
               | PSSleeping
               | PSWaiting
               | PSZombie
               | PSStopped
               | PSTracing
               | PSPaging
               | PSDead
               | PSWakekill
               | PSWaking
               | PSParked
               deriving (Show)
instance ReadC8 ProcState where
  readC8 s = lookup s mapping
    where
      mapping :: [(C8.ByteString, ProcState)]
      mapping = [ (C8.singleton 'R', PSRunning)
                , (C8.singleton 'S', PSSleeping)
                , (C8.singleton 'D', PSWaiting)
                , (C8.singleton 'Z', PSZombie)
                , (C8.singleton 'T', PSStopped)
                , (C8.singleton 't', PSTracing)
--                , (C8.singleton 'W', PSPaging)
                , (C8.singleton 'X', PSDead)
                , (C8.singleton 'x', PSDead)
                , (C8.singleton 'K', PSWakekill)
                , (C8.singleton 'W', PSWaking)
                , (C8.singleton 'P', PSParked)]

data DeviceNumber = DN { dnMajor :: Word8
                       , dnMinor :: Word32 }
                  deriving (Show)
instance ReadC8 DeviceNumber where
  readC8 s = do
    (n, r) <- C8.readInteger s
    if C8.empty == r
      then do
        let major = shiftR n 8 .&. 255
            minorL = n .&. 255
            minorH = shiftR n 19 .&. 8191
            minor = shiftL minorH 8 .|. minorL
        return $ DN (fromIntegral major) (fromIntegral minor)
      else if C8.length r > 1 && C8.head r == ':'
      then do
        m <- readIntegerAll $ C8.tail r
        return $ DN (fromIntegral n) (fromIntegral m)
      else fail ""

data ProcessFlag = PFIdle
                 | PFExiting
                 | PFExitpidone
                 | PFVcpu
                 | PFWqWorker
                 | PFForknoexec
                 | PFMceProcess
                 | PFSuperpriv
                 | PFDumpcore
                 | PFSignaled
                 | PFMemalloc
                 | PFNprocExceeded
                 | PFUsedMath
                 | PFUsedAsync
                 | PFNofreeze
                 | PFFrozen
                 | PFKswapd
                 | PFMemallocNofs
                 | PFMemallocNoio
                 | PFLessThrottle
                 | PFKthread
                 | PFRandomize
                 | PFSwapwrite
                 | PFMemstall
                 | PFUmh
                 | PFNoSetaffinity
                 | PFMceEarly
                 | PFMutexTester
                 | PFFreezerSkip
                 | PFSuspendTask
                 deriving (Show)
instance Enum ProcessFlag where
  fromEnum PFIdle = 0x00000002
  fromEnum PFExiting = 0x00000004
  fromEnum PFExitpidone = 0x00000008
  fromEnum PFVcpu = 0x00000010
  fromEnum PFWqWorker = 0x00000020
  fromEnum PFForknoexec = 0x00000040
  fromEnum PFMceProcess = 0x00000080
  fromEnum PFSuperpriv = 0x00000100
  fromEnum PFDumpcore = 0x00000200
  fromEnum PFSignaled = 0x00000400
  fromEnum PFMemalloc = 0x00000800
  fromEnum PFNprocExceeded = 0x00001000
  fromEnum PFUsedMath = 0x00002000
  fromEnum PFUsedAsync = 0x00004000
  fromEnum PFNofreeze = 0x00008000
  fromEnum PFFrozen = 0x00010000
  fromEnum PFKswapd = 0x00020000
  fromEnum PFMemallocNofs = 0x00040000
  fromEnum PFMemallocNoio = 0x00080000
  fromEnum PFLessThrottle = 0x00100000
  fromEnum PFKthread = 0x00200000
  fromEnum PFRandomize = 0x00400000
  fromEnum PFSwapwrite = 0x00800000
  fromEnum PFMemstall = 0x01000000
  fromEnum PFUmh = 0x02000000
  fromEnum PFNoSetaffinity = 0x04000000
  fromEnum PFMceEarly = 0x08000000
  fromEnum PFMutexTester = 0x20000000
  fromEnum PFFreezerSkip = 0x40000000
  fromEnum PFSuspendTask = 0x80000000
  toEnum 0x00000002 = PFIdle
  toEnum 0x00000004 = PFExiting
  toEnum 0x00000008 = PFExitpidone
  toEnum 0x00000010 = PFVcpu
  toEnum 0x00000020 = PFWqWorker
  toEnum 0x00000040 = PFForknoexec
  toEnum 0x00000080 = PFMceProcess
  toEnum 0x00000100 = PFSuperpriv
  toEnum 0x00000200 = PFDumpcore
  toEnum 0x00000400 = PFSignaled
  toEnum 0x00000800 = PFMemalloc
  toEnum 0x00001000 = PFNprocExceeded
  toEnum 0x00002000 = PFUsedMath
  toEnum 0x00004000 = PFUsedAsync
  toEnum 0x00008000 = PFNofreeze
  toEnum 0x00010000 = PFFrozen
  toEnum 0x00020000 = PFKswapd
  toEnum 0x00040000 = PFMemallocNofs
  toEnum 0x00080000 = PFMemallocNoio
  toEnum 0x00100000 = PFLessThrottle
  toEnum 0x00200000 = PFKthread
  toEnum 0x00400000 = PFRandomize
  toEnum 0x00800000 = PFSwapwrite
  toEnum 0x01000000 = PFMemstall
  toEnum 0x02000000 = PFUmh
  toEnum 0x04000000 = PFNoSetaffinity
  toEnum 0x08000000 = PFMceEarly
  toEnum 0x20000000 = PFMutexTester
  toEnum 0x40000000 = PFFreezerSkip
  toEnum 0x80000000 = PFSuspendTask
  toEnum _ = undefined
readProcessFlags :: C8.ByteString -> Maybe [ProcessFlag]
readProcessFlags flags = do
  flags' <- readIntegerAll flags
  return $ filter (\enum -> fromEnum enum .&. fromIntegral flags' == fromEnum enum) $
    map (toEnum . shiftL 1) $ [1..27] ++ [29, 30, 31]

data SchedPolicy = SPNormal
                 | SPFifo
                 | SPRr
                 | SPBatch
                 | SPIso
                 | SPIdle
                 | SPDeadline
                 deriving (Show)
instance Enum SchedPolicy where
  fromEnum SPNormal = 0
  fromEnum SPFifo = 1
  fromEnum SPRr = 2
  fromEnum SPBatch = 3
  fromEnum SPIso = 4
  fromEnum SPIdle = 5
  fromEnum SPDeadline = 6
  toEnum 0 = SPNormal
  toEnum 1 = SPFifo
  toEnum 2 = SPRr
  toEnum 3 = SPBatch
  toEnum 4 = SPIso
  toEnum 5 = SPIdle
  toEnum 6 = SPDeadline
  toEnum _ = undefined

data ExitStatus = ESExited ExitCode
                | ESSignaled Signal
                | ESCoreDump
                | ESStoppped Signal
                | ESContinued
                deriving (Eq, Show)
instance ReadC8 ExitStatus where
  readC8 s = do
    i <- readIntegerAll s
    let termsig = i .&. 0x7F
        exited = termsig == 0
        exitstatus = shiftR (i .&. 0xFF00) 8
        signaled = shiftR (termsig + 1) 1 > 0
        coredump = i .&. coreflag /= 0
        stopped = i .&. 0xFF == 0x7F
        stopsig = exitstatus
        continued = i == 0xFFFF
    if exited
      then return $ ESExited $ case exitstatus of
        0 -> ExitSuccess
        j -> ExitFailure $ fromIntegral j
      else if signaled
      then return $ ESSignaled $ fromIntegral termsig
      else if coredump
      then return ESCoreDump
      else if stopped
      then return $ ESStoppped $ fromIntegral stopsig
      else if continued
      then return ESContinued
      else fail ""
    where
      coreflag = 0x80

instance ReadC8 Int where
  readC8 = fmap fromIntegral . readIntegerAll

instance ReadC8 Signal where
  readC8 = fmap fromIntegral . readIntegerAll

instance ReadC8 Integer where
  readC8 = readIntegerAll

instance ReadC8 ProcessID where
  readC8 = fmap fromIntegral . readIntegerAll

type ProcessSessionID = CPid
type ClockTicks = Integer
type Address = Integer
type CPUId = Integer

data Stat = Stat { statPid :: ProcessID
                 , statComm :: String
                 , statState :: ProcState
                 , statPpid :: ProcessID
                 , statPgrp :: ProcessGroupID
                 , statSession :: ProcessSessionID
                 , statTtyNr :: DeviceNumber
                 , statTpgid :: Maybe ProcessGroupID
                 , statFlags :: [ProcessFlag]
                 , statMinflt :: Integer
                 , statCminflt :: Integer
                 , statMajflt :: Integer
                 , statCmajflt :: Integer
                 , statUtime :: ClockTicks
                 , statStime :: ClockTicks
                 , statCutime :: ClockTicks
                 , statCstime :: ClockTicks
                 , statPriority :: Integer
                 , statNice :: Integer
                 , statNumThreads :: Integer
                 , statItrealvalue :: ()
                 , statStarttime :: ClockTicks
                 , statVsize :: Integer
                 , statRss :: Integer
                 , statRsslim :: Integer
                 , statStartcode :: Maybe Address
                 , statEndcode :: Maybe Address
                 , statStartstack :: Maybe Address
                 , statKstkesp :: Maybe Address
                 , statKstkeip :: Maybe Address
                 , statSignal :: ()
                 , statBlocked :: ()
                 , statSigignore :: ()
                 , statSigcatch :: ()
                 , statWchan :: Maybe Integer
                 , statNswap :: ()
                 , statCnswap :: ()
                 , statExitSignal :: Signal
                 , statProcesser :: CPUId
                 , statRtPriority :: Integer
                 , statPolicy :: SchedPolicy
                 , statDelayacctBlkioTicks :: ClockTicks
                 , statGuestTime :: ClockTicks
                 , statCguestTime :: ClockTicks
                 , statStartData :: Maybe Address
                 , statEndData :: Maybe Address
                 , statStartBrk :: Maybe Address
                 , statArgStart :: Maybe Address
                 , statArgEnd :: Maybe Address
                 , statEnvStart :: Maybe Address
                 , statEnvEnd :: Maybe Address
                 , statExitCode :: Maybe ExitStatus }
          deriving (Show)
instance ReadC8 Stat where
  readC8 s = do
    lp <- C8.elemIndex '(' s
    rp <- C8.elemIndexEnd ')' s
    let pid' = C8.take (lp - 1) s
        comm' = C8.unpack $ C8.drop (lp + 1) $ C8.take rp s
        fields = [C8.empty, C8.empty] ++ C8.split ' ' (C8.drop (rp + 1) s)
    pure Stat
      <*> readC8 pid'
      <*> pure comm'
      <*> readC8 (fields !! 2)
      <*> readC8 (fields !! 3)
      <*> readC8 (fields !! 4)
      <*> readC8 (fields !! 5)
      <*> readC8 (fields !! 6)
      <*> fmap (nothingOnValue (-1)) (readC8 (fields !! 7))
      <*> readProcessFlags (fields !! 8)
      <*> readC8 (fields !! 9)
      <*> readC8 (fields !! 10)
      <*> readC8 (fields !! 11)
      <*> readC8 (fields !! 12)
      <*> readC8 (fields !! 13)
      <*> readC8 (fields !! 14)
      <*> readC8 (fields !! 15)
      <*> readC8 (fields !! 16)
      <*> readC8 (fields !! 17)
      <*> readC8 (fields !! 18)
      <*> readC8 (fields !! 19)
      <*> pure ()
      <*> readC8 (fields !! 21)
      <*> readC8 (fields !! 22)
      <*> readC8 (fields !! 23)
      <*> readC8 (fields !! 24)
      <*> fmap (nothingOnValue 0) (readC8 (fields !! 25))
      <*> fmap (nothingOnValue 0) (readC8 (fields !! 26))
      <*> fmap (nothingOnValue 0) (readC8 (fields !! 27))
      <*> fmap (nothingOnValue 0) (readC8 (fields !! 28))
      <*> fmap (nothingOnValue 0) (readC8 (fields !! 29))
      <*> pure ()
      <*> pure ()
      <*> pure ()
      <*> pure ()
      <*> fmap (nothingOnValue 0) (readC8 (fields !! 34))
      <*> pure ()
      <*> pure ()
      <*> readC8 (fields !! 37)
      <*> readC8 (fields !! 38)
      <*> readC8 (fields !! 39)
      <*> (toEnum <$> readC8 (fields !! 40))
      <*> readC8 (fields !! 41)
      <*> readC8 (fields !! 42)
      <*> readC8 (fields !! 43)
      <*> fmap (nothingOnValue 0) (readC8 (fields !! 44))
      <*> fmap (nothingOnValue 0) (readC8 (fields !! 45))
      <*> fmap (nothingOnValue 0) (readC8 (fields !! 46))
      <*> fmap (nothingOnValue 0) (readC8 (fields !! 47))
      <*> fmap (nothingOnValue 0) (readC8 (fields !! 48))
      <*> fmap (nothingOnValue 0) (readC8 (fields !! 49))
      <*> fmap (nothingOnValue 0) (readC8 (fields !! 50))
      <*> fmap (nothingOnValue (ESExited ExitSuccess)) (readC8 (fields !! 51))
      where
        nothingOnValue :: (Eq a) => a -> a -> Maybe a
        nothingOnValue x y = if x == y
          then Nothing
          else Just y
