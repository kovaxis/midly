//! There's an abomination called RMID, MIDI embedded in a RIFF file.
//! Support for these files is provided by unwrapping the input slice, stripping away the RIFF
//! wrappers around the raw SMF file.

use crate::prelude::*;

struct ChunkIter<'a>(&'a mut [u8]);
impl<'a> Iterator for ChunkIter<'a> {
    type Item = ([u8; 4], &'a mut [u8]);
    fn next(&mut self) -> Option<([u8; 4], &'a mut [u8])> {
        let chunkhead = read_slice(&mut self.0, 8).ok()?;
        let mut id = [0; 4];
        let mut len = [0; 4];
        id.copy_from_slice(&chunkhead[..4]);
        len.copy_from_slice(&chunkhead[4..8]);
        let len = u32::from_le_bytes(len);
        let data = match read_slice(&mut self.0, len as usize) {
            Ok(data) => data,
            Err(_) => mem::replace(&mut self.0, &mut []),
        };
        if len % 2 == 1 {
            let _pad = read_slice(&mut self.0, 1);
        }
        Some((id, data))
    }
}

pub fn unwrap(raw: &mut [u8]) -> Result<&mut [u8]> {
    let (id, mut riff) = ChunkIter(raw)
        .next()
        .ok_or(err_invalid!("no main riff chunk"))?;
    if &id != b"RIFF" {
        bail!(err_invalid!("invalid main riff chunk"));
    }
    let formtype =
        read_slice(&mut riff, 4).context(err_invalid!("failed to read riff formtype"))?;
    if formtype != b"RMID" {
        bail!(err_invalid!("riff formtype is not rmid"));
    }
    for (id, chunk) in ChunkIter(riff) {
        if &id == b"data" {
            return Ok(chunk);
        }
    }
    bail!(err_invalid!("no rmid data chunk"))
}
