use nix::sys::signal::Signal;
use nix::unistd::Pid;
use num_traits::Num;
use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

#[derive(Debug)]
pub struct StringParseError;
#[derive(Debug)]
pub struct NoneError;
impl From<ParseIntError> for StringParseError {
  fn from(_: ParseIntError) -> Self {
    StringParseError
  }
}
impl From<nix::Error> for StringParseError {
  fn from(_: nix::Error) -> Self {
    StringParseError
  }
}
impl From<NoneError> for StringParseError {
  fn from(_: NoneError) -> Self {
    StringParseError
  }
}

#[derive(Clone)]
pub struct Pid_ {
  pub pid: Pid,
}
impl FromStr for Pid_ {
  type Err = StringParseError;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    Ok(Pid_ {
      pid: Pid::from_raw(i32::from_str(s)?),
    })
  }
}
impl fmt::Display for Pid_ {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.pid.fmt(f)
  }
}
impl std::cmp::PartialEq for Pid_ {
  fn eq(&self, other: &Self) -> bool {
    self.pid == other.pid
  }
}
impl std::cmp::Eq for Pid_ {}

#[derive(Clone)]
pub enum ProcStat {
  PSRunning,
  PSSleeping,
  PSWaiting,
  PSZombie,
  PSStopped,
  PSTracing,
  PSPaging,
  PSDead,
  PSWakekill,
  PSWaking,
  PSParked,
}
impl FromStr for ProcStat {
  type Err = StringParseError;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "R" => Ok(ProcStat::PSRunning),
      "S" => Ok(ProcStat::PSSleeping),
      "D" => Ok(ProcStat::PSWaiting),
      "Z" => Ok(ProcStat::PSZombie),
      "T" => Ok(ProcStat::PSStopped),
      "t" => Ok(ProcStat::PSTracing),
      // "W" => Ok(ProcStat::PSPaging),
      "X" => Ok(ProcStat::PSDead),
      "x" => Ok(ProcStat::PSDead),
      "K" => Ok(ProcStat::PSWakekill),
      "W" => Ok(ProcStat::PSWaking),
      "P" => Ok(ProcStat::PSParked),
      _ => Err(StringParseError),
    }
  }
}

#[derive(Clone)]
pub struct DeviceNumber {
  pub major: u8,
  pub minor: u32,
}
impl FromStr for DeviceNumber {
  type Err = StringParseError;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let s_: Vec<_> = s.splitn(2, ':').collect();
    if s_.len() == 2 {
      let major = u8::from_str(s_[0])?;
      let minor = u32::from_str(s_[1])?;
      Ok(DeviceNumber {
        major: major,
        minor: minor,
      })
    } else {
      let n = u32::from_str(s)?;
      let major = (n >> 8) & 255;
      let minor_l = n & 255;
      let minor_h = (n >> 19) & 8191;
      let minor = (minor_h << 8) | minor_l;
      Ok(DeviceNumber {
        major: major as u8,
        minor: minor,
      })
    }
  }
}

bitflags! {
  pub struct ProcessFlag: u32 {
    const PFIDLE = 0x00000002;
    const PFEXITING = 0x00000004;
    const PFEXITPIDONE = 0x00000008;
    const PFVCPU = 0x00000010;
    const PFWQWORKER = 0x00000020;
    const PFFORKNOEXEC = 0x00000040;
    const PFMCEPROCESS = 0x00000080;
    const PFSUPERPRIV = 0x00000100;
    const PFDUMPCORE = 0x00000200;
    const PFSIGNALED = 0x00000400;
    const PFMEMALLOC = 0x00000800;
    const PFNPROCEXCEEDED = 0x00001000;
    const PFUSEDMATH = 0x00002000;
    const PFUSEDASYNC = 0x00004000;
    const PFNOFREEZE = 0x00008000;
    const PFFROZEN = 0x00010000;
    const PFKSWAPD = 0x00020000;
    const PFMEMALLOCNOFS = 0x00040000;
    const PFMEMALLOCNOIO = 0x00080000;
    const PFLESSTHROTTLE = 0x00100000;
    const PFKTHREAD = 0x00200000;
    const PFRANDOMIZE = 0x00400000;
    const PFSWAPWRITE = 0x00800000;
    const PFMEMSTALL = 0x01000000;
    const PFUMH = 0x02000000;
    const PFNOSETAFFINITY = 0x04000000;
    const PFMCEEARLY = 0x08000000;
    const PFMUTEXTESTER = 0x20000000;
    const PFFREEZERSKIP = 0x40000000;
    const PFSUSPENDTASK = 0x80000000;
  }
}
impl FromStr for ProcessFlag {
  type Err = StringParseError;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    Ok(o2r(ProcessFlag::from_bits(u32::from_str(s)?))?)
  }
}

#[derive(Clone)]
pub enum SchedPolicy {
  SPNormal = 0,
  SPFifo = 1,
  SPRr = 2,
  SPBatch = 3,
  SPIso = 4,
  SPIdle = 5,
  SPDeadline = 6,
}
impl FromStr for SchedPolicy {
  type Err = StringParseError;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "0" => Ok(SchedPolicy::SPNormal),
      "1" => Ok(SchedPolicy::SPFifo),
      "2" => Ok(SchedPolicy::SPRr),
      "3" => Ok(SchedPolicy::SPBatch),
      "4" => Ok(SchedPolicy::SPIso),
      "5" => Ok(SchedPolicy::SPIdle),
      "6" => Ok(SchedPolicy::SPDeadline),
      _ => Err(StringParseError),
    }
  }
}

pub enum ExitStatus {
  ESExited(u8),
  ESSignaled(Signal),
  ESCoreDump,
  ESStopped(Signal),
  ESContinued,
}
impl FromStr for ExitStatus {
  type Err = StringParseError;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let i = u64::from_str(s)?;
    let termsig = i & 0x7f;
    let exited = termsig == 0;
    let exitstatus = (i & 0xff00) >> 8;
    let signaled = (termsig + 1) >> 1 > 0;
    let coredump = i & 0x80 != 0;
    let stopped = i & 0xff == 0x7f;
    let stopsig = exitstatus;
    let continued = i == 0xffff;
    if exited {
      Ok(ExitStatus::ESExited(exitstatus as u8))
    } else if signaled {
      Ok(ExitStatus::ESSignaled(Signal::from_c_int(termsig as i32)?))
    } else if coredump {
      Ok(ExitStatus::ESCoreDump)
    } else if stopped {
      Ok(ExitStatus::ESStopped(Signal::from_c_int(stopsig as i32)?))
    } else if continued {
      Ok(ExitStatus::ESContinued)
    } else {
      Err(StringParseError)
    }
  }
}

pub type ClockTicksU = u64;
pub type ClockTicksI = i64;
pub type Address = u64;
pub type CPUId = u32;
pub type ProcessSessionId = Pid_;
pub type ProcessGroupId = Pid_;

fn from_ptrace_protected<T: Num>(s: T) -> Option<T> {
  if s.is_zero() {
    None
  } else {
    Some(s)
  }
}

fn o2r<T>(o: Option<T>) -> Result<T, NoneError> {
  if let Some(x) = o {
    Ok(x)
  } else {
    Err(NoneError)
  }
}

fn nat_or_none<U, E, F: FnOnce(i32) -> Result<U, E>>(n: i32, f: F) -> Result<Option<U>, E> {
  Ok(if n > 0 { Some(f(n)?) } else { None })
}

fn drop<Iter: Iterator>(iter: &mut Iter) -> () {
  iter.next();
}

// %d (%s) %c %d %d %d %d %d %u %lu
// %lu %lu %lu %lu %lu %ld %ld %ld %ld %ld
// %ld %llu %lu %ld %lu %lu %lu %lu %lu %lu
// %lu %lu %lu %lu %lu %lu %lu %d %d $u
// %u %llu %lu %ld %lu %lu %lu %lu %lu %lu
// %lu %d
#[derive(Clone)]
pub struct Stat {
  pub pid: Pid_,
  pub comm: String,
  pub state: ProcStat,
  pub ppid: Pid_,
  pub pgrp: ProcessGroupId,
  pub session: ProcessSessionId,
  pub tty_nr: DeviceNumber,
  pub tpgid: Option<ProcessGroupId>,
  pub flags: ProcessFlag,
  pub minflt: u64,

  pub cminflt: u64,
  pub majflt: u64,
  pub cmajflt: u64,
  pub utime: ClockTicksU,
  pub stime: ClockTicksU,
  pub cutime: ClockTicksI,
  pub cstime: ClockTicksI,
  pub priority: i64,
  pub nice: i64,
  pub num_threads: i64,

  pub itrealvalue: (),
  pub starttime: ClockTicksU,
  pub vsize: u64,
  pub rss: i64,
  pub rsslim: u64,
  pub startcode: Option<Address>,
  pub endcode: Option<Address>,
  pub startstack: Option<Address>,
  pub kstkesp: Option<Address>,
  pub kstkeip: Option<Address>,

  pub signal: (),
  pub blocked: (),
  pub sigignore: (),
  pub sigcatch: (),
  pub wchan: Option<u64>,
  pub nswap: (),
  pub cnswap: (),
  pub exit_signal: Signal,
  pub processer: CPUId,
  pub rt_priority: u32,

  pub policy: SchedPolicy,
  pub delayacct_blkio_ticks: ClockTicksU,
  pub guest_time: ClockTicksU,
  pub cguest_time: ClockTicksI,
  pub start_data: Option<Address>,
  pub end_data: Option<Address>,
  pub start_brk: Option<Address>,
  pub arg_start: Option<Address>,
  pub arg_end: Option<Address>,
  pub env_start: Option<Address>,

  pub env_end: Option<Address>,
  pub exit_code: Option<i32>,
}
impl FromStr for Stat {
  type Err = StringParseError;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let lp = o2r(s.find('('))?;
    let rp = o2r(s.rfind(')'))?;
    let mut parts = s[(rp + 2)..].split(' ');
    Ok(Stat {
      pid: Pid_::from_str(&s[0..(lp - 1)])?,
      comm: s[(lp + 1)..rp].to_string(),
      state: ProcStat::from_str(o2r(parts.next())?)?,
      ppid: Pid_::from_str(o2r(parts.next())?)?,
      pgrp: Pid_::from_str(o2r(parts.next())?)?,
      session: Pid_::from_str(o2r(parts.next())?)?,
      tty_nr: DeviceNumber::from_str(o2r(parts.next())?)?,
      tpgid: nat_or_none::<_, StringParseError, _>(i32::from_str(o2r(parts.next())?)?, |i| {
        Ok(Pid_ {
          pid: Pid::from_raw(i),
        })
      })?,
      flags: ProcessFlag::from_str(o2r(parts.next())?)?,
      minflt: u64::from_str(o2r(parts.next())?)?,

      cminflt: u64::from_str(o2r(parts.next())?)?,
      majflt: u64::from_str(o2r(parts.next())?)?,
      cmajflt: u64::from_str(o2r(parts.next())?)?,
      utime: u64::from_str(o2r(parts.next())?)?,
      stime: u64::from_str(o2r(parts.next())?)?,
      cutime: i64::from_str(o2r(parts.next())?)?,
      cstime: i64::from_str(o2r(parts.next())?)?,
      priority: i64::from_str(o2r(parts.next())?)?,
      nice: i64::from_str(o2r(parts.next())?)?,
      num_threads: i64::from_str(o2r(parts.next())?)?,

      itrealvalue: drop(&mut parts),
      starttime: u64::from_str(o2r(parts.next())?)?,
      vsize: u64::from_str(o2r(parts.next())?)?,
      rss: i64::from_str(o2r(parts.next())?)?,
      rsslim: u64::from_str(o2r(parts.next())?)?,
      startcode: from_ptrace_protected(u64::from_str(o2r(parts.next())?)?),
      endcode: from_ptrace_protected(u64::from_str(o2r(parts.next())?)?),
      startstack: from_ptrace_protected(u64::from_str(o2r(parts.next())?)?),
      kstkesp: from_ptrace_protected(u64::from_str(o2r(parts.next())?)?),
      kstkeip: from_ptrace_protected(u64::from_str(o2r(parts.next())?)?),

      signal: drop(&mut parts),
      blocked: drop(&mut parts),
      sigignore: drop(&mut parts),
      sigcatch: drop(&mut parts),
      wchan: from_ptrace_protected(u64::from_str(o2r(parts.next())?)?),
      nswap: drop(&mut parts),
      cnswap: drop(&mut parts),
      exit_signal: Signal::from_c_int(i32::from_str(o2r(parts.next())?)?)?,
      processer: u32::from_str(o2r(parts.next())?)?,
      rt_priority: u32::from_str(o2r(parts.next())?)?,

      policy: SchedPolicy::from_str(o2r(parts.next())?)?,
      delayacct_blkio_ticks: u64::from_str(o2r(parts.next())?)?,
      guest_time: u64::from_str(o2r(parts.next())?)?,
      cguest_time: i64::from_str(o2r(parts.next())?)?,
      start_data: from_ptrace_protected(u64::from_str(o2r(parts.next())?)?),
      end_data: from_ptrace_protected(u64::from_str(o2r(parts.next())?)?),
      start_brk: from_ptrace_protected(u64::from_str(o2r(parts.next())?)?),
      arg_start: from_ptrace_protected(u64::from_str(o2r(parts.next())?)?),
      arg_end: from_ptrace_protected(u64::from_str(o2r(parts.next())?)?),
      env_start: from_ptrace_protected(u64::from_str(o2r(parts.next())?)?),

      env_end: from_ptrace_protected(u64::from_str(o2r(parts.next())?)?),
      exit_code: from_ptrace_protected(i32::from_str(o2r(parts.next())?.trim_end())?),
    })
  }
}
