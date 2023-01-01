use crate::{stack_buffer, Buffer, DefaultBuffer};

#[test]
fn default_buf() {
    let mut buf = DefaultBuffer::default();
    buf.push(&[123, 143]).unwrap();
    buf.push(&[]).unwrap();
    buf.push(&[15]).unwrap();
    assert_eq!(buf.as_slice(), &[123, 143, 15]);
    buf.clear();
    buf.push(&[14]).unwrap();
    assert_eq!(buf.as_slice(), &[14]);
    let buf_copy = buf.clone();
    assert_eq!(buf.push(&vec![0; 1024 * 1024]), Err(()));
    assert_eq!(buf.as_slice(), buf_copy.as_slice());
    assert_eq!(format!("{:?}", buf), format!("{:?}", buf_copy));
}

#[test]
fn stack_buf() {
    stack_buffer! {
        struct Buf([u8; 16 * 1024]);
    }
    let mut buf = Buf::new();
    buf.push(&[123, 143]).unwrap();
    buf.push(&[]).unwrap();
    buf.push(&[15]).unwrap();
    assert_eq!(buf.as_slice(), &[123, 143, 15]);
    buf.clear();
    buf.push(&[14]).unwrap();
    assert_eq!(buf.as_slice(), &[14]);
    let buf_copy = buf.clone();
    assert_eq!(buf.push(&vec![0; 16 * 1024]), Err(()));
    assert_eq!(buf.as_slice(), buf_copy.as_slice());
    assert_eq!(format!("{:?}", buf), format!("{:?}", buf_copy));
}
