use {SmfBuffer,Event,TrackIter};
use std::time::{Instant};

#[test]
fn parse_test() {
  let smf=SmfBuffer::open("test-asset/test.midi").unwrap();
  let smf=smf.parse::<Vec<Event>>().unwrap();
  println!("{} tracks",smf.tracks.len());
}
#[test]
fn parse_pi() {
  let start=Instant::now();
  let smf=SmfBuffer::open("test-asset/Pi.mid").unwrap();
  let smf=smf.parse::<TrackIter>().unwrap();
  let counts: Vec<_>=smf.tracks.into_iter().map(|track| track.count()).collect();
  for (i,count) in counts.iter().enumerate() {
    println!("track {} has {} events",i,count);
  }
  let took=Instant::now()-start;
  println!("took {}ms parsing pi",(took*1000).as_secs());
}