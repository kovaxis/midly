use crate::prelude::*;
use crossbeam_channel::{self as channel, Receiver, Sender};
use midly_smf::Smf;

#[derive(Debug)]
pub enum PlayError<E> {
    Parse(&'static str),
    Consumer(E),
}
impl<E> fmt::Display for PlayError<E>
where
    E: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Parse(msg) => write!(f, "error parsing midi file: {}", msg),
            Self::Consumer(e) => fmt::Display::fmt(e, f),
        }
    }
}
impl<E> StdError for PlayError<E>
where
    E: StdError + 'static,
{
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            Self::Parse(_) => None,
            Self::Consumer(e) => Some(e),
        }
    }
}

pub enum FilePlayError<E> {
    Io(io::Error),
    Play(PlayError<E>),
}
impl<E> fmt::Debug for FilePlayError<E>
where
    E: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Io(e) => f.debug_tuple("Io").field(e).finish(),
            Self::Play(e) => f.debug_tuple("Play").field(e).finish(),
        }
    }
}
impl<E> fmt::Display for FilePlayError<E>
where
    E: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Io(e) => write!(f, "error reading midi file: {}", e),
            Self::Play(e) => fmt::Display::fmt(e, f),
        }
    }
}
impl<E> StdError for FilePlayError<E>
where
    E: StdError + 'static,
{
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            Self::Io(e) => Some(e),
            Self::Play(e) => Some(e),
        }
    }
}

enum PlayCommand {
    Play,
    Pause(Instant),
    Seek(f64),
    Stop,
}

#[derive(Debug, Clone)]
pub struct PlayHandle {
    cmd: Sender<PlayCommand>,
    cmd_recv: Receiver<PlayCommand>,
}
impl PlayHandle {
    pub fn empty() -> PlayHandle {
        let (cmd, cmd_recv) = channel::bounded(8);
        Self { cmd, cmd_recv }
    }
    pub fn play(&mut self) {
        let _ = self.cmd.send(PlayCommand::Play);
    }
    pub fn pause(&mut self) {
        let _ = self.cmd.send(PlayCommand::Pause(Instant::now()));
    }
    #[doc(hidden)]
    pub fn seek(&mut self, pos: f64) {
        let _ = self.cmd.send(PlayCommand::Seek(pos));
    }
    pub fn stop(&mut self) {
        let _ = self.cmd.send(PlayCommand::Stop);
    }
}

pub fn play_path<P, M, F, E>(path: P, send_to: M, on_finish: F) -> PlayHandle
where
    P: AsRef<Path>,
    M: FnMut(Instant, MidiMessage) -> Result<(), E> + Send + 'static,
    F: FnOnce(Result<(), FilePlayError<E>>) + Send + 'static,
{
    let file = match File::open(path) {
        Ok(f) => f,
        Err(err) => {
            on_finish(Err(FilePlayError::Io(err)));
            return PlayHandle::empty();
        }
    };
    play_file(file, send_to, on_finish)
}

pub fn play_file<M, F, E>(file: File, send_to: M, on_finish: F) -> PlayHandle
where
    M: FnMut(Instant, MidiMessage) -> Result<(), E> + Send + 'static,
    F: FnOnce(Result<(), FilePlayError<E>>) + Send + 'static,
{
    let size_hint = usize::try_from(file.metadata().map(|meta| meta.len()).unwrap_or(0))
        .unwrap_or(isize::max_value() as usize);
    play_reader(file, size_hint, send_to, on_finish)
}

pub fn play_reader<R, M, F, E>(
    mut read: R,
    size_hint: usize,
    send_to: M,
    on_finish: F,
) -> PlayHandle
where
    R: Read,
    M: FnMut(Instant, MidiMessage) -> Result<(), E> + Send + 'static,
    F: FnOnce(Result<(), FilePlayError<E>>) + Send + 'static,
{
    let mut raw = Vec::with_capacity(size_hint);
    if let Err(err) = read.read_to_end(&mut raw) {
        on_finish(Err(FilePlayError::Io(err)));
        return PlayHandle::empty();
    }
    drop(read);
    play_bytes(raw, send_to, |err| match err {
        Ok(()) => return on_finish(Ok(())),
        Err(e) => on_finish(Err(FilePlayError::Play(e))),
    })
}

pub fn play_bytes<M, F, E>(raw: Vec<u8>, send_to: M, on_finish: F) -> PlayHandle
where
    M: FnMut(Instant, MidiMessage) -> Result<(), E> + Send + 'static,
    F: FnOnce(Result<(), PlayError<E>>) + Send + 'static,
{
    let handle = PlayHandle::empty();
    let handle2 = handle.clone();
    thread::spawn(move || match run_bytes(raw, send_to, Some(handle2)) {
        Ok(()) => on_finish(Ok(())),
        Err(err) => on_finish(Err(err)),
    });
    handle
}

pub fn run_bytes<M, E>(
    mut raw: Vec<u8>,
    send_to: M,
    handle: Option<PlayHandle>,
) -> Result<(), PlayError<E>>
where
    M: FnMut(Instant, MidiMessage) -> Result<(), E>,
{
    let smf = Smf::parse(&mut raw).map_err(|e| PlayError::Parse(e.kind().message()))?;
    run_smf(smf, send_to, handle).map_err(|e| PlayError::Consumer(e))
}

struct PlayState<M> {
    handle: PlayHandle,
    last_time: Instant,
    paused: bool,
    send_to: M,
}
impl<M, E> PlayState<M>
where
    M: FnMut(Instant, MidiMessage) -> Result<(), E>,
{
    fn new(send_to: M, handle: PlayHandle) -> Self {
        Self {
            handle,
            last_time: Instant::now(),
            paused: false,
            send_to,
        }
    }

    fn handle_cmd(&mut self, cmd: PlayCommand) -> bool {
        match cmd {
            PlayCommand::Play => self.paused = false,
            PlayCommand::Pause(at) => {
                if !self.paused {
                    self.paused = true;
                    while self.paused {
                        match self.handle.cmd_recv.recv() {
                            Ok(cmd) => {
                                if self.handle_cmd(cmd) {
                                    return true;
                                }
                            }
                            Err(_) => break,
                        }
                    }
                    self.last_time += Instant::now() - at;
                }
            }
            PlayCommand::Seek(_) => {
                //TODO: Implement seeking
            }
            PlayCommand::Stop => {
                return true;
            }
        }
        false
    }

    fn run(&mut self, smf: Smf) -> Result<(), E> {
        'outer: for (interval, msg) in smf.into_timed_iter() {
            self.last_time += interval;
            let ev_time = loop {
                let ev_time = self.last_time;
                let now = Instant::now();
                if now < ev_time {
                    spin_sleep::sleep(ev_time - now);
                }
                while let Ok(cmd) = self.handle.cmd_recv.try_recv() {
                    if self.handle_cmd(cmd) {
                        break 'outer;
                    }
                }
                if self.last_time == ev_time {
                    break ev_time;
                }
            };
            (self.send_to)(ev_time, msg)?;
        }
        Ok(())
    }
}

fn run_smf<M, E>(smf: Smf, send_to: M, handle: Option<PlayHandle>) -> Result<(), E>
where
    M: FnMut(Instant, MidiMessage) -> Result<(), E>,
{
    let mut state = PlayState::new(send_to, handle.unwrap_or_else(|| PlayHandle::empty()));
    state.run(smf)
}
