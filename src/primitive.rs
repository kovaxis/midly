use prelude::*;

pub trait SplitChecked: Sized {
  fn split_checked(&mut self,usize)->Option<Self>;
}
impl<'a> SplitChecked for &'a [u8] {
  fn split_checked(&mut self,at: usize)->Option<&'a [u8]> {
    if at>self.len() {
      None
    }else{
      let (extracted,remainder)=self.split_at(at);
      *self=remainder;
      Some(extracted)
    }
  }
}

///Implemented on integer types for reading as big-endian
pub trait IntRead: Sized {
  ///Reads a big-endian integer
  fn read(&mut &[u8])->Result<Self>;
}
///Reads the int from u7 bytes, that is, top bit in all bytes is ignored
///For raw reading on integer types, use `read_raw`
pub trait IntReadBottom7: Sized {
  ///Read an int from bytes, but only using the bottom 7 bits of each byte
  fn read_u7(&mut &[u8])->Result<Self>;
}

//Implement simple big endian integer reads
macro_rules! impl_read_int {
  {$( $int:ty ),*} => {
    $(
      impl IntRead for $int {
        fn read(raw: &mut &[u8])->Result<$int> {
          let bytes=raw.split_checked(::std::mem::size_of::<$int>())
            .ok_or("failed to read the expected integer")?;
          Ok(bytes.iter().fold(0,|mut acc,byte| {
            acc=acc.checked_shl(8).unwrap_or(0);
            acc|=*byte as $int;
            acc
          }))
        }
      }
    )*
  }
}
impl_read_int! {u8,u16,u32}

//Slightly restricted integers
macro_rules! int_feature {
  { $name:ident ; $inner:tt : read_u7 }=>{
    impl IntReadBottom7 for $name {
      fn read_u7(raw: &mut &[u8])->Result<$name> {
        let bytes=raw.split_checked(::std::mem::size_of::<$inner>())
          .ok_or("failed to read the expected integer")?;
        ensure!(bytes.iter().all(|byte| !byte.bit(7)),"invalid byte with top bit set");
        Ok(
          Self::try_from(
            bytes.iter().fold(0,|mut acc,byte| {
              acc<<=7;
              acc|=*byte as $inner;
              acc
            })
          ).ok_or(stringify!("expected " $name ", found " $inner))?
        )
      }
    }
  };
  { $name:ident ; $inner:tt : read }=>{
    impl IntRead for $name {
      fn read(raw: &mut &[u8])->Result<Self> {
        Ok(Self::try_from( $inner ::read(raw)?).ok_or(stringify!("expected " $name ", found " $inner))?)
      }
    }
  };
}
macro_rules! restricted_int {
  {$name:ident : $inner:tt => $bits:expr ; $( $feature:tt )* } => {
    #[derive(Copy,Clone,Debug)]
    #[allow(non_camel_case_types)]
    pub struct $name($inner);
    impl From<$inner> for $name {
      ///Lossy convertion, loses top bit
      fn from(raw: $inner)->Self {
        $name (raw.bit_range(0..$bits))
      }
    }
    impl Into<$inner> for $name {
      fn into(self)->$inner {self.0}
    }
    impl $name {
      pub fn try_from(raw: $inner)->Option<Self> {
        if raw.bit($bits) {
          None
        }else{
          Some($name(raw))
        }
      }
    }
    $( int_feature!{$name ; $inner : $feature} )*
  };
}
restricted_int!{u15: u16 => 15; read}
restricted_int!{u14: u16 => 14; read read_u7}
restricted_int!{u7: u8 => 7; read}
restricted_int!{u4: u8 => 4; read}
restricted_int!{u2: u8 => 2; read}
restricted_int!{u24: u32 => 24;}
impl IntRead for u24 {
  fn read(raw: &mut &[u8])->Result<u24> {
    let bytes=raw.split_checked(3).ok_or("failed to read u24 bytes")?;
    //Using lossy `from` because value is guaranteed to be 24 bits (3 bytes)
    Ok(u24::from(bytes.iter().fold(0,|mut acc,byte| {
      acc<<=8;
      acc|=*byte as u32;
      acc
    })))
  }
}

///A variable length int (as specified by MIDI)
restricted_int!{Varlen: u32 => 28;}
impl IntReadBottom7 for Varlen {
  fn read_u7(raw: &mut &[u8])->Result<Varlen> {
    let mut int: u32=0;
    for byte_index in 0..4 {
      let byte=raw.split_checked(1).ok_or("unexpected eof when reading varlen integer")?[0];
      int<<=7;
      int|=byte.bit_range(0..7) as u32;
      if byte.bit(7) {
        if byte_index==3 {
          bail!("varlen integer larger than 4 bytes")
        }
      }else{
        break
      }
    }
    //Since we did at max 4 reads of 7 bits each, there MUST be at max 28 bits in this int
    //Therefore it's safe to call lossy `from`
    Ok(Varlen::from(int))
  }
}

///Reads a slice represented in the input as a `Varlen` `len` followed by `len` bytes
pub fn read_varlen_slice<'a>(raw: &mut &'a [u8])->Result<&'a [u8]> {
  let len: u32=Varlen::read_u7(raw).chain_err(|| "failed to read varlen slice length")?.into();
  Ok(raw.split_checked(len as usize).ok_or("incomplete varlen slice")?)
}

///The different formats an SMF file can be
#[derive(Copy,Clone,Debug)]
pub enum Format {
  SingleTrack,
  Parallel,
  Sequential,
}
impl Format {
  pub fn read(raw: &mut &[u8])->Result<Format> {
    let format=u16::read(raw)?;
    Ok(match format {
      0=>Format::SingleTrack,
      1=>Format::Parallel,
      2=>Format::Sequential,
      _=>bail!("invalid smf format")
    })
  }
}

///The timing for an SMF file
///This can be in ticks/beat or ticks/second
#[derive(Copy,Clone,Debug)]
pub enum Timing {
  ///Specifies ticks/beat as a 15-bit integer
  Metrical(u15),
  ///Specifies ticks/second by dividing a second into frames and then into subframes
  ///Therefore the length of of a tick is `1/fps/subframe`
  Timecode(Fps,u8)
}
impl Timing {
  pub fn read(raw: &mut &[u8])->Result<Timing> {
    let raw=u16::read(raw).chain_err(|| "unexpected eof when reading midi timing")?;
    if raw.bit(15) {
      //Timecode
      let fps=-(raw.bit_range(8..16) as i8);
      let subframe=raw.bit_range(0..8) as u8;
      Ok(Timing::Timecode(Fps::from_u8(fps as u8).ok_or("invalid smpte fps")?,subframe))
    }else{
      //Metrical
      Ok(Timing::Metrical(u15::from(raw)))
    }
  }
}

///Encodes an SMPTE time of the day
///Enforces several guarantees:
///`hour` is inside [0,23]
///`minute` is inside [0,59]
///`second` is inside [0,59]
///`frame` is inside [0,fps[
///`subframe` is inside [0,99]
#[derive(Copy,Clone,Debug)]
pub struct SmpteTime {
  hour: u8,
  minute: u8,
  second: u8,
  frame: u8,
  subframe: u8,
  fps: Fps,
}
impl SmpteTime {
  pub fn new(hour: u8,minute: u8,second: u8,frame: u8,subframe: u8,fps: Fps)->Option<SmpteTime> {
    macro_rules! check {($cond:expr)=>{{if !{$cond} {return None}}}}
    check!(hour<24);
    check!(minute<60);
    check!(second<60);
    check!(frame<fps.as_u8());
    check!(subframe<100);
    Some(SmpteTime{hour,minute,second,frame,subframe,fps})
  }
  pub fn hour(&self)->u8 {self.hour}
  pub fn minute(&self)->u8 {self.minute}
  pub fn second(&self)->u8 {self.second}
  pub fn frame(&self)->u8 {self.frame}
  pub fn subframe(&self)->u8 {self.subframe}
  pub fn fps(&self)->Fps {self.fps}
  pub fn second_f32(&self)->f32 {
    self.second as f32+((self.frame as f32+self.subframe as f32/100.0)/self.fps.as_f32())
  }
  pub fn read(raw: &mut &[u8])->Result<SmpteTime> {
    let data=raw.split_checked(5).ok_or("failed to read smpte time data")?;
    let hour=data[0];
    let (hour,fps)=(hour.bit_range(0..5),hour.bit_range(5..7).into());
    let fps=Fps::from_code(fps);
    let minute=data[1];
    let second=data[2];
    let frame=data[3];
    let subframe=data[4];
    Ok(SmpteTime::new(hour,minute,second,frame,subframe,fps).ok_or("invalid smpte time")?)
  }
}

#[derive(Copy,Clone,Debug)]
pub enum Fps {
  Fps24,
  Fps25,
  Fps29,
  Fps30,
}
impl Fps {
  ///Does transformation from 2-bit fps code
  fn from_code(code: u2)->Fps {
    match code.into() {
      0=>Fps::Fps24,
      1=>Fps::Fps25,
      2=>Fps::Fps29,
      3=>Fps::Fps30,
      _=>unreachable!()
    }
  }
  ///Does direct transformation (ie. 24 -> Fps24)
  fn from_u8(raw: u8)->Option<Fps> {
    Some(match raw {
      24=>Fps::Fps24,
      25=>Fps::Fps25,
      29=>Fps::Fps29,
      30=>Fps::Fps30,
      _=>return None
    })
  }
  ///Get the numerical approximate fps out
  fn as_u8(self)->u8 {
    match self {
      Fps::Fps24=>24,
      Fps::Fps25=>25,
      Fps::Fps29=>29,
      Fps::Fps30=>30,
    }
  }
  ///Get the actual f32 fps out
  fn as_f32(self)->f32 {
    match self.as_u8() {
      24=>24.0,
      25=>25.0,
      29=>30.0/1.001,
      30=>30.0,
      _=>unreachable!()
    }
  }
}