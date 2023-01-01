use crate::prelude::*;

#[inline]
pub fn advance_slice<'a>(raw: &mut &'a mut [u8], len: usize) -> &'a mut [u8] {
    let (take, rem) = mem::replace(raw, &mut []).split_at_mut(len);
    *raw = rem;
    take
}

#[inline]
pub fn read_slice<'a>(raw: &mut &'a mut [u8], len: usize) -> Result<&'a mut [u8]> {
    ensure!(raw.len() >= len, err_invalid!("unexpected eof"));
    Ok(advance_slice(raw, len))
}

#[inline]
pub fn read_u8(raw: &mut &mut [u8]) -> Result<u8> {
    Ok(read_slice(raw, 1)?[0])
}

#[inline]
pub fn read_u16(raw: &mut &mut [u8]) -> Result<u16> {
    let buf = read_slice(raw, 2)?;
    let buf = [buf[0], buf[1]];
    Ok(u16::from_be_bytes(buf))
}

#[inline]
pub fn read_u32(raw: &mut &mut [u8]) -> Result<u32> {
    let buf = read_slice(raw, 4)?;
    let buf = [buf[0], buf[1], buf[2], buf[3]];
    Ok(u32::from_be_bytes(buf))
}

#[inline]
pub fn read_varlen(raw: &mut &mut [u8]) -> Result<u32> {
    let mut int: u32 = 0;
    for _ in 0..4 {
        let byte = match read_u8(raw).context(err_malformed!("truncated varlen")) {
            Ok(b) => b,
            Err(e) => {
                if cfg!(feature = "strict") {
                    bail!(e)
                } else {
                    // Stay with what was read
                    break;
                }
            }
        };
        int <<= 7;
        int |= (byte & 0x7F) as u32;
        if byte & 0x80 == 0 {
            return Ok(int);
        }
    }
    if cfg!(feature = "strict") {
        bail!(err_malformed!("varlen integer larger than 4 bytes"))
    } else {
        // Use the 4 bytes as-is
        Ok(int)
    }
}

#[inline]
pub fn read_varlen_noconsume<'a>(mut raw: &'a mut [u8]) -> Result<(u32, usize)> {
    let origlen = raw.len();
    let int = read_varlen(&mut raw)?;
    Ok((int, origlen - raw.len()))
}

#[inline]
pub fn write_varlen<W: Write>(out: &mut W, int: u32) -> WriteResult<W> {
    let mut skipping = true;
    for i in (0..4).rev() {
        let byte = ((int >> (i * 7)) & 0x7F) as u8;
        if skipping && byte == 0 && i != 0 {
            // Skip these leading zeros
        } else {
            // Write down this u7
            skipping = false;
            let byte = if i == 0 {
                // Last byte
                byte
            } else {
                // Leading byte
                byte | 0x80
            };
            out.write(&[byte])?;
        }
    }
    Ok(())
}

#[inline]
pub fn read_varlen_slice<'a>(raw: &mut &'a mut [u8]) -> Result<&'a mut [u8]> {
    let len = read_varlen(raw)?;
    match read_slice(raw, len as usize).context(err_malformed!("truncated varlen slice")) {
        Ok(s) => Ok(s),
        Err(e) => {
            if cfg!(feature = "strict") {
                bail!(e)
            } else {
                Ok(mem::replace(raw, &mut []))
            }
        }
    }
}

#[inline]
pub fn write_varlen_slice<W: Write>(out: &mut W, data: &[u8]) -> WriteResult<W> {
    let len = data.len();
    if len >= 1 << 28 {
        bail!(W::invalid_input("varlen slice length exceeds 28 bits"));
    }
    write_varlen(out, len as u32)?;
    out.write(data)
}
