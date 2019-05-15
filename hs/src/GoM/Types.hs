{-# LANGUAGE LambdaCase           #-}
module GoM.Types where

import Debug.Trace
import Data.Bits
import Data.List
import Data.Maybe
import Data.Word
import GoM.Helper
import System.Exit
import System.Posix.Signals
import System.Posix.Types
import Text.Read
import Text.Read.Lex

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
instance Read ProcState where
  readPrec = get >>= return . \case
    'R' -> PSRunning
    'S' -> PSSleeping
    'D' -> PSWaiting
    'Z' -> PSZombie
    'T' -> PSStopped
    't' -> PSTracing
    -- 'W' -> PSPaging
    'X' -> PSDead
    'x' -> PSDead
    'K' -> PSWakekill
    'W' -> PSWaking
    'P' -> PSParked
    _ -> undefined

data DeviceNumber = DN { dnMajor :: Word8
                       , dnMinor :: Word32 }
                  deriving (Show)
instance Read DeviceNumber where
  readPrec = colon +++ number
    where
      colon = do
        Number major <- lexP
        Symbol ":" <- lexP
        Number minor <- lexP
        return $ DN (fromIntegral $ fromJust $ numberToInteger major) (fromIntegral $ fromJust $ numberToInteger minor)
      number = do
        Number n' <- lexP
        let Just n = numberToInteger n'
            major = shiftR n 8 .&. 255
            minorL = n .&. 255
            minorH = shiftR n 19 .&. 8191
            minor = shiftL minorH 8 .|. minorL
        return $ DN (fromIntegral major) (fromIntegral minor)

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
readProcessFlags :: String -> Maybe [ProcessFlag]
readProcessFlags flags = do
  flags' <- readMaybe flags
  return $ filter (\enum -> fromEnum enum .&. flags' == fromEnum enum) $ map (toEnum . shiftL 1) $ [1..27] ++ [29, 30, 31]

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
                deriving (Show)
instance Read ExitStatus where
  readPrec = do
    Number i' <- lexP
    let i = fromIntegral $ fromJust $ numberToInteger i'
        termsig = i .&. 0x7F
        exited = termsig == 0
        exitstatus = shiftR (i .&. 0xFF00) 8
        signaled = shiftR (termsig + 1) 1 > 0
        coredump = i .&. coreflag /= 0
        stopped = i .&. 0xFF == 0x7F
        stopsig = exitstatus
        continued = i == 0xFFFF
    return $ if exited
      then ESExited $ case exitstatus of
        0 -> ExitSuccess
        j -> ExitFailure j
      else if signaled
      then ESSignaled $ fromIntegral termsig
      else if coredump
      then ESCoreDump
      else if stopped
      then ESStoppped $ fromIntegral stopsig
      else if continued
      then ESContinued
      else undefined
    where
      coreflag = 0x80

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
                 , statTpgid :: ProcessGroupID
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
instance Read Stat where
  readPrec = do
    String str <- lexP
    EOF <- lexP
    return $ fromJust $ parseStat str
parseStat :: String -> Maybe Stat
parseStat str = do
  lp <- elemIndex '(' str
  rp <- elemIndex ')' str
  let pid' = take (lp - 1) str
      comm' = drop (lp + 1) $ take rp str
      fields = ["", ""] ++ splitOn (drop (rp + 1) str) ' '
  pure Stat
    <*> readMaybe pid'
    <*> pure comm'
    <*> readMaybe (fields !! 2)
    <*> readMaybe (fields !! 3)
    <*> readMaybe (fields !! 4)
    <*> readMaybe (fields !! 5)
    <*> readMaybe (fields !! 6)
    <*> readMaybe (fields !! 7)
    <*> readProcessFlags (fields !! 8)
    <*> readMaybe (fields !! 9)
    <*> readMaybe (fields !! 10)
    <*> readMaybe (fields !! 11)
    <*> readMaybe (fields !! 12)
    <*> readMaybe (fields !! 13)
    <*> readMaybe (fields !! 14)
    <*> readMaybe (fields !! 15)
    <*> readMaybe (fields !! 16)
    <*> readMaybe (fields !! 17)
    <*> readMaybe (fields !! 18)
    <*> readMaybe (fields !! 19)
    <*> pure ()
    <*> readMaybe (fields !! 21)
    <*> readMaybe (fields !! 22)
    <*> readMaybe (fields !! 23)
    <*> readMaybe (fields !! 24)
    <*> rm (fields !! 25)
    <*> rm (fields !! 26)
    <*> rm (fields !! 27)
    <*> rm (fields !! 28)
    <*> rm (fields !! 29)
    <*> pure ()
    <*> pure ()
    <*> pure ()
    <*> pure ()
    <*> rm (fields !! 34)
    <*> pure ()
    <*> pure ()
    <*> readMaybe (fields !! 37)
    <*> readMaybe (fields !! 38)
    <*> readMaybe (fields !! 39)
    <*> (toEnum <$> readMaybe (fields !! 40))
    <*> readMaybe (fields !! 41)
    <*> readMaybe (fields !! 42)
    <*> readMaybe (fields !! 43)
    <*> rm (fields !! 44)
    <*> rm (fields !! 45)
    <*> rm (fields !! 46)
    <*> rm (fields !! 47)
    <*> rm (fields !! 48)
    <*> rm (fields !! 49)
    <*> rm (fields !! 50)
    <*> rm (fields !! 51)
    where
      rm :: (Read a) => String -> Maybe (Maybe a)
      rm = \case
        "0" -> Just Nothing
        i -> case readMaybe i of
          Nothing -> Nothing
          Just i' -> Just $ Just i'
