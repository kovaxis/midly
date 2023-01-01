use crate::{
    prelude::*,
    smf::{Format, Smf, Timing},
    Header, TrackEvent,
};
use alloc::collections::BinaryHeap;
use core::cmp::Reverse;

pub struct TimedIter<'a> {
    inner: GenericTimedIter<&'a Smf<'a>>,
}
impl<'a> TimedIter<'a> {
    pub fn new(smf: &'a Smf<'a>) -> TimedIter<'a> {
        Self {
            inner: GenericTimedIter::new(smf),
        }
    }
}
impl<'a> Iterator for TimedIter<'a> {
    type Item = (Duration, &'a MidiMessage<'a>);
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

pub struct TimedIterMut<'a> {
    inner: GenericTimedIter<&'a mut Smf<'a>>,
}
impl<'a> TimedIterMut<'a> {
    pub fn new(smf: &'a mut Smf<'a>) -> TimedIterMut<'a> {
        Self {
            inner: GenericTimedIter::new(smf),
        }
    }
}
impl<'a> Iterator for TimedIterMut<'a> {
    type Item = (Duration, &'a mut MidiMessage<'a>);
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

pub struct TimedIntoIter<'a> {
    inner: GenericTimedIter<Smf<'a>>,
}
impl<'a> TimedIntoIter<'a> {
    pub fn new(smf: Smf<'a>) -> TimedIntoIter<'a> {
        Self {
            inner: GenericTimedIter::new(smf),
        }
    }
}
impl<'a> Iterator for TimedIntoIter<'a> {
    type Item = (Duration, MidiMessage<'a>);
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

struct GenericTimedIter<T> {
    ev_iter: GenericOrderedIter<T>,
    ticks_per_beat: Option<u16>,
    tick_duration: Duration,
}
impl<T: SmfBorrow> GenericTimedIter<T> {
    pub fn new(smf: T) -> GenericTimedIter<T> {
        let (ticks_per_beat, tick_duration) = match smf.header().timing {
            Timing::Metrical(tpb) => (Some(tpb), Duration::from_millis(500) / tpb as u32),
            Timing::Timecode(fps, subframe) => {
                use midly_core::Fps::*;
                let sec = Duration::from_secs(1);
                (
                    None,
                    match fps {
                        Fps24 => sec / 24,
                        Fps25 => sec / 25,
                        Fps2997 => sec * 1001 / 30000,
                        Fps30 => sec / 30,
                    } / subframe as u32,
                )
            }
        };
        Self {
            ev_iter: GenericOrderedIter::new(smf),
            ticks_per_beat,
            tick_duration,
        }
    }
}
impl<T: SmfBorrow> Iterator for GenericTimedIter<T> {
    type Item = (Duration, T::Message);
    fn next(&mut self) -> Option<Self::Item> {
        let (tick_interval, msg) = self.ev_iter.next()?;
        let interval = tick_interval * self.tick_duration;
        match T::borrow_msg(&msg) {
            MidiMessage::Tempo(beat) => {
                if let Some(tpb) = self.ticks_per_beat {
                    self.tick_duration = Duration::from_micros(*beat as u64) / tpb as u32;
                }
            }
            _ => {}
        }
        Some((interval, msg))
    }
}

pub struct OrderedIter<'a> {
    inner: GenericOrderedIter<&'a Smf<'a>>,
}
impl<'a> OrderedIter<'a> {
    pub fn new(smf: &'a Smf<'a>) -> OrderedIter<'a> {
        Self {
            inner: GenericOrderedIter::new(smf),
        }
    }
}
impl<'a> Iterator for OrderedIter<'a> {
    type Item = (u32, &'a MidiMessage<'a>);
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

pub struct OrderedIterMut<'a> {
    inner: GenericOrderedIter<&'a mut Smf<'a>>,
}
impl<'a> OrderedIterMut<'a> {
    pub fn new(smf: &'a mut Smf<'a>) -> OrderedIterMut<'a> {
        Self {
            inner: GenericOrderedIter::new(smf),
        }
    }
}
impl<'a> Iterator for OrderedIterMut<'a> {
    type Item = (u32, &'a mut MidiMessage<'a>);
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

pub struct OrderedIntoIter<'a> {
    inner: GenericOrderedIter<Smf<'a>>,
}
impl<'a> OrderedIntoIter<'a> {
    pub fn new(smf: Smf<'a>) -> OrderedIntoIter<'a> {
        Self {
            inner: GenericOrderedIter::new(smf),
        }
    }
}
impl<'a> Iterator for OrderedIntoIter<'a> {
    type Item = (u32, MidiMessage<'a>);
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

#[derive(Debug, Clone)]
enum GenericOrderedIter<T> {
    Parallel(ParallelIter<T>),
    Sequential(SequentialIter<T>),
}
impl<T: SmfBorrow> GenericOrderedIter<T> {
    pub fn new(smf: T) -> GenericOrderedIter<T> {
        match smf.header().format {
            Format::Parallel => Self::Parallel(ParallelIter::new(smf)),
            Format::Sequential | Format::SingleTrack => Self::Sequential(SequentialIter::new(smf)),
        }
    }
}
impl<T: SmfBorrow> Iterator for GenericOrderedIter<T> {
    type Item = (u32, T::Message);
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Parallel(iter) => iter.next(),
            Self::Sequential(iter) => iter.next(),
        }
    }
}

#[derive(Debug, Clone)]
struct TrackState {
    time: u64,
    cur_idx: usize,
}

#[derive(Debug, Clone)]
struct ParallelIter<T> {
    smf: T,
    track_state: Vec<TrackState>,
    ev_heap: BinaryHeap<Reverse<(u64, usize)>>,
    last_tick: u64,
}
impl<T: SmfBorrow> ParallelIter<T> {
    pub fn new(smf: T) -> ParallelIter<T> {
        let mut ev_heap = BinaryHeap::with_capacity(smf.track_count());
        for track_idx in 0..smf.track_count() {
            if let Some(delta) = smf.peek_delta(track_idx, 0) {
                ev_heap.push(Reverse((delta, track_idx)));
            }
        }
        Self {
            track_state: vec![
                TrackState {
                    time: 0,
                    cur_idx: 0
                };
                smf.track_count()
            ],
            ev_heap,
            smf,
            last_tick: 0,
        }
    }
}
impl<T: SmfBorrow> Iterator for ParallelIter<T> {
    type Item = (u32, T::Message);
    fn next(&mut self) -> Option<Self::Item> {
        let Reverse((time, track_idx)) = self.ev_heap.pop()?;
        // Get this event and add next event to heap
        let tstate = &mut self.track_state[track_idx];
        let this_ev = unsafe {
            let ev = self.smf.next_event(track_idx, tstate.cur_idx);
            tstate.cur_idx += 1;
            ev.unwrap()
        };
        tstate.time = time;
        if let Some(delta) = self.smf.peek_delta(track_idx, tstate.cur_idx) {
            self.ev_heap.push(Reverse((time + delta, track_idx)));
        }
        let deltatime = (time - self.last_tick) as u32;
        self.last_tick = time;
        Some((deltatime, T::message(this_ev)))
    }
}

#[derive(Debug, Clone)]
struct SequentialIter<T> {
    smf: T,
    track_idx: usize,
    ev_idx: usize,
}
impl<T> SequentialIter<T> {
    pub fn new(smf: T) -> SequentialIter<T> {
        Self {
            smf,
            track_idx: 0,
            ev_idx: 0,
        }
    }
}
impl<T: SmfBorrow> Iterator for SequentialIter<T> {
    type Item = (u32, T::Message);
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.track_idx >= self.smf.track_count() {
                break None;
            }
            unsafe {
                match self.smf.next_event(self.track_idx, self.ev_idx) {
                    Some(ev) => {
                        self.ev_idx += 1;
                        break Some((T::delta(&ev) as u32, T::message(ev)));
                    }
                    None => {
                        self.ev_idx = 0;
                        self.track_idx += 1;
                    }
                }
            }
        }
    }
}

trait SmfBorrow {
    type Event;
    type Message;
    fn header(&self) -> &Header;
    fn track_count(&self) -> usize;
    fn peek_delta(&self, track_idx: usize, ev_idx: usize) -> Option<u64>;
    unsafe fn next_event(&mut self, track_idx: usize, ev_idx: usize) -> Option<Self::Event>;
    fn delta(ev: &Self::Event) -> u64;
    fn message(ev: Self::Event) -> Self::Message;
    fn borrow_msg(msg: &Self::Message) -> &MidiMessage;
}
impl<'a> SmfBorrow for &'a Smf<'a> {
    type Event = &'a TrackEvent<'a>;
    type Message = &'a MidiMessage<'a>;
    fn header(&self) -> &Header {
        &self.header
    }
    fn track_count(&self) -> usize {
        self.tracks.len()
    }
    fn peek_delta(&self, track_idx: usize, ev_idx: usize) -> Option<u64> {
        self.tracks[track_idx]
            .events
            .get(ev_idx)
            .map(|ev| ev.delta as u64)
    }
    unsafe fn next_event(&mut self, track_idx: usize, ev_idx: usize) -> Option<Self::Event> {
        self.tracks[track_idx].events.get(ev_idx)
    }
    fn delta(ev: &Self::Event) -> u64 {
        ev.delta as u64
    }
    fn message(ev: Self::Event) -> Self::Message {
        &ev.msg
    }
    fn borrow_msg(msg: &Self::Message) -> &MidiMessage {
        msg
    }
}
impl<'a> SmfBorrow for Smf<'a> {
    type Event = TrackEvent<'a>;
    type Message = MidiMessage<'a>;
    fn header(&self) -> &Header {
        &self.header
    }
    fn track_count(&self) -> usize {
        self.tracks.len()
    }
    fn peek_delta(&self, track_idx: usize, ev_idx: usize) -> Option<u64> {
        self.tracks[track_idx]
            .events
            .get(ev_idx)
            .map(|ev| ev.delta as u64)
    }
    /// SAFETY: Must never be called with the same `(track_idx, ev_idx)` pair twice.
    unsafe fn next_event(&mut self, track_idx: usize, ev_idx: usize) -> Option<Self::Event> {
        self.tracks[track_idx].events.get_mut(ev_idx).map(|ev| {
            mem::replace(
                ev,
                TrackEvent {
                    delta: 0,
                    msg: MidiMessage::Reset,
                },
            )
        })
    }
    fn delta(ev: &Self::Event) -> u64 {
        ev.delta as u64
    }
    fn message(ev: Self::Event) -> Self::Message {
        ev.msg
    }
    fn borrow_msg(msg: &Self::Message) -> &MidiMessage {
        msg
    }
}
impl<'a> SmfBorrow for &'a mut Smf<'a> {
    type Event = &'a mut TrackEvent<'a>;
    type Message = &'a mut MidiMessage<'a>;
    fn header(&self) -> &Header {
        &self.header
    }
    fn track_count(&self) -> usize {
        self.tracks.len()
    }
    fn peek_delta(&self, track_idx: usize, ev_idx: usize) -> Option<u64> {
        self.tracks[track_idx]
            .events
            .get(ev_idx)
            .map(|ev| ev.delta as u64)
    }
    unsafe fn next_event(&mut self, track_idx: usize, ev_idx: usize) -> Option<Self::Event> {
        // SAFETY: Unsafe magic is required to promise the borrow checker that two mutable
        // references will never be made to the same location. This is possible because the safety
        // contract of `next_event` (a function that is unsafe to call) requires that it never be
        // called with the same `(track_idx, ev_idx)` pair twice.
        self.tracks[track_idx]
            .events
            .get_mut(ev_idx)
            .map(|ev_ref| &mut *(ev_ref as *mut TrackEvent<'a>))
    }
    fn delta(ev: &Self::Event) -> u64 {
        ev.delta as u64
    }
    fn message(ev: Self::Event) -> Self::Message {
        &mut ev.msg
    }
    fn borrow_msg(msg: &Self::Message) -> &MidiMessage {
        msg
    }
}
