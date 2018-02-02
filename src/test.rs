use SmfBuffer;
use std::time::{Instant};

///Open
macro_rules! open {
  {$name:ident : $file:expr}=>{
    let $name=SmfBuffer::open(format!("test-asset/{}",$file)).unwrap();
  }
}
///Standardized testing
macro_rules! test {
  {($name:expr , $file:expr) => {$method_parse:ident,$method_len:ident}}=>{{
    let counts=time($name,||->Vec<_> {
      open!{smf: $file};
      let smf=smf. $method_parse ().unwrap();
      smf.tracks.into_iter().map(|track| track. $method_len ()).collect()
    });
    for (i,count) in counts.iter().enumerate() {
      println!("track {} has {} events",i,count);
    }
  }}
}
///Timing
fn time<F: FnOnce()->R,R>(activity: &str,op: F)->R {
  let start=Instant::now();
  let result=op();
  let took=Instant::now()-start;
  println!("{}: {}ms",activity,(took*1000).as_secs());
  result
}

///Parsing
mod parse {
  use super::*;
  
  #[test]
  fn clementi_defer() {
    test!(("parse_clementi_iter","Clementi.mid")=>{parse_defer,count});
  }
  #[test]
  fn clementi_collect() {
    test!(("parse_clementi_vec","Clementi.mid")=>{parse_collect,len});
  }
  #[test]
  fn pi_defer() {
    test!(("parse_pi_iter","Pi.mid")=>{parse_defer,count});
  }
  #[test]
  fn pi_collect() {
    test!(("parse_pi_vec","Pi.mid")=>{parse_collect,len});
  }
}