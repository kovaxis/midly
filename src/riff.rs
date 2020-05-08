//! There's an abomination called RMID, MIDI embedded in a RIFF file.
//! Support for these files is provided by unwrapping the input slice, stripping away the RIFF
//! wrappers around the raw SMF file.

use crate::prelude::*;

struct ChunkIter<'a>(&'a [u8]);
impl<'a> Iterator for ChunkIter<'a> {
    type Item = ([u8; 4], &'a [u8]);
    fn next(&mut self) -> Option<([u8; 4], &'a [u8])> {
        if self.0.len() >= 8 {
            let mut id = [0; 4];
            let mut len = [0; 4];
            id.copy_from_slice(&self.0[..4]);
            len.copy_from_slice(&self.0[4..8]);
            self.0 = &self.0[8..];
            let len = u32::from_le_bytes(len);
            let data = match self.0.split_checked(len as usize) {
                Some(data) => data,
                None => mem::replace(&mut self.0, &[]),
            };
            if len % 2 == 1 {
                let _pad = self.0.split_checked(1);
            }
            Some((id, data))
        } else {
            None
        }
    }
}

pub fn unwrap(raw: &[u8]) -> Result<&[u8]> {
    let (id, mut riff) = ChunkIter(raw)
        .next()
        .ok_or(err_invalid!("no main riff chunk"))?;
    if &id != b"RIFF" {
        bail!(err_invalid!("invalid main riff chunk"));
    }
    let formtype = riff
        .split_checked(4)
        .ok_or(err_invalid!("failed to read riff formtype"))?;
    if formtype != b"RMID" {
        bail!(err_invalid!("not an rmid riff file"));
    }
    for (id, chunk) in ChunkIter(riff) {
        if &id == b"data" {
            return Ok(chunk);
        }
    }
    bail!(err_invalid!("no rmid data chunk"))
}
