extern crate notify_send;
extern crate procfs;
use handled::*;
use nix::sys::signal::{kill, SIGTERM};
use notify_send::notify;
use procfs::*;
use std::fs::{read_dir, read_to_string};
use std::os::unix::fs::MetadataExt;
use std::thread::sleep;
use std::time::Duration;
use users::{get_current_uid, uid_t};

fn main() {
    let limit0s = 393216;
    let limit0h = 524288;
    let me = get_current_uid();
    let mut handled: HandledDict = Handled::new();
    loop {
        watch(me, limit0s, limit0h, &mut handled);
        sleep(Duration::from_secs(1))
    }
}

fn watch<T: Handled>(me: uid_t, ls: i64, lh: i64, handled: &mut T) {
    for entry_ in read_dir("/proc").unwrap() {
        let _ = entry_.and_then(|entry| {
            entry.metadata().and_then(|metadata| {
                if entry.path().is_dir()
                    && entry
                        .file_name()
                        .into_string()
                        .unwrap()
                        .parse::<u64>()
                        .is_ok()
                    && metadata.uid() == me
                {
                    read_to_string(entry.path().join("stat")).and_then(|s| {
                        Ok({
                            let stat = Stat::from_str(&s)
                                .expect(&format!("Content {} cannot be parsed as stat.", &s));
                            if stat.rss >= lh && !handled.is_hard_handled(&stat) {
                                println!("{} hardlimit", stat.comm);
                                notify_(&format!(
                                    "{}|{} reached hard limit",
                                    &stat.pid, &stat.comm
                                ));
                                if let Err(e) = kill(stat.pid.pid, SIGTERM) {
                                    println!(
                                        "Killing {}|{} failed with {}",
                                        stat.pid, stat.comm, e
                                    );
                                }
                                handled.add_hard(&stat);
                            } else if stat.rss >= ls && !handled.is_soft_handled(&stat) {
                                println!("{} softlimit", stat.comm);
                                notify_(&format!("{}|{} reached soft limit", stat.pid, stat.comm));
                                handled.add_soft(&stat);
                            } else {
                                handled.del(&stat);
                            }
                        })
                    })
                } else {
                    Ok(())
                }
            })
        });
    }
}

fn notify_(msg: &str) {
    if let Err(e) = notify("GoM", None, "", msg, "", 10000) {
        println!("Sending notification faile with {}", e);
    }
}

pub mod handled {
    use procfs::*;
    pub trait Handled {
        fn new() -> Self;
        fn add_soft(&mut self, stat: &Stat) -> ();
        fn add_hard(&mut self, stat: &Stat) -> ();
        fn del(&mut self, stat: &Stat) -> ();
        fn is_soft_handled(&self, stat: &Stat) -> bool;
        fn is_hard_handled(&self, stat: &Stat) -> bool;
    }
    pub type HandledDict = (Vec<Stat>, Vec<Stat>);
    impl Handled for HandledDict {
        fn new() -> Self {
            (Vec::new(), Vec::new())
        }
        fn add_soft(&mut self, stat: &Stat) {
            self.del(stat);
            let (s, _) = self;
            if let None = s
                .iter()
                .position(|x| x.pid == stat.pid && x.comm == stat.comm)
            {
                s.push((*stat).clone());
            }
        }
        fn add_hard(&mut self, stat: &Stat) {
            self.del(stat);
            let (_, h) = self;
            if let None = h
                .iter()
                .position(|x| x.pid == stat.pid && x.comm == stat.comm)
            {
                h.push((*stat).clone());
            }
        }
        fn del(&mut self, stat: &Stat) {
            let (s, h) = self;
            if let Some(i) = h
                .iter()
                .position(|x| x.pid == stat.pid && x.comm == stat.comm)
            {
                h.remove(i);
            }
            if let Some(i) = s
                .iter()
                .position(|x| x.pid == stat.pid && x.comm == stat.comm)
            {
                s.remove(i);
            }
        }
        fn is_soft_handled(&self, stat: &Stat) -> bool {
            let (s, _) = self;
            if let Some(_) = s
                .iter()
                .position(|x| x.pid == stat.pid && x.comm == stat.comm)
            {
                true
            } else {
                false
            }
        }
        fn is_hard_handled(&self, stat: &Stat) -> bool {
            let (_, h) = self;
            if let Some(_) = h
                .iter()
                .position(|x| x.pid == stat.pid && x.comm == stat.comm)
            {
                true
            } else {
                false
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::handled::*;
    use procfs::*;
    #[test]
    fn test() {
        let mut t: HandledDict = Handled::new();
        let s = Stat::from_str("30842 (cat) R 28714 30842 28714 34816 30842 4194304 106 0 0 0 0 0 0 0 20 0 1 0 6369123 5550080 55 18446744073709551615 93870926708736 93870926733201 140735431914848 0 0 0 0 0 0 0 0 0 17 2 0 0 0 0 0 93870926748368 93870926749888 93870947766272 140735431920918 140735431920938 140735431920938 140735431925739 0").unwrap();
        t.add_soft(&s);
        assert_eq!(t.is_soft_handled(&s), true);
        assert_eq!(t.is_hard_handled(&s), false);
        t.add_hard(&s);
        assert_eq!(t.is_soft_handled(&s), false);
        assert_eq!(t.is_hard_handled(&s), true);
        t.del(&s);
        assert_eq!(t.is_soft_handled(&s), false);
        assert_eq!(t.is_hard_handled(&s), false);
    }
}
