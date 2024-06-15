#![cfg(feature = "alloc")]

use crate::prelude::*;
use core::cell::UnsafeCell;

/// Helps overcome limitations of the lifetime system when constructing MIDI events and files.
///
/// Because many events contain references to data that outlives them, it can be hard to build a
/// MIDI file programmatically.
///
/// Consider the following code:
///
/// ```rust,compile_fail
/// use midly::{TrackEvent, TrackEventKind, MetaMessage};
///
/// let mut track = Vec::new();
/// for i in 0..64 {
///     let marker_name = format!("Marker {}", i);
///     let marker_ref = marker_name.as_bytes();
///     track.push(TrackEvent {
///         delta: 0.into(),
///         kind: TrackEventKind::Meta(MetaMessage::Marker(marker_ref)),
///     });
/// }
/// ```
///
/// Looks pretty good, but it fails to compile with
/// `error[E0597]: "marker_name" does not live long enough`, with a rightful reason: `marker_name`
/// is dropped before the next iteration of the `for` loop.
///
/// Instead, use the [`Arena`](struct.Arena.html) type like the following code:
///
/// ```rust
/// use midly::{TrackEvent, TrackEventKind, MetaMessage};
///
/// let arena = midly::Arena::new();
/// let mut track = Vec::new();
/// for i in 0..64 {
///     let marker_name = format!("Marker {}", i);
///     let marker_ref = arena.add(marker_name.as_bytes());
///     track.push(TrackEvent {
///         delta: 0.into(),
///         kind: TrackEventKind::Meta(MetaMessage::Marker(marker_ref)),
///     });
/// }
/// ```
///
/// This type is only available with the `alloc` feature enabled.
#[derive(Default)]
pub struct Arena {
    allocations: UnsafeCell<Vec<*mut [u8]>>,
}
impl Arena {
    /// Create a new empty arena.
    #[inline]
    pub fn new() -> Arena {
        Self::default()
    }

    /// Empty this arena, deallocating all added bytes.
    ///
    /// This method is safe to call because it requires a mutable reference.
    #[inline]
    pub fn clear(&mut self) {
        // SAFETY:
        // Accessing the `UnsafeCell` is safe because we have a mutable reference to it.
        // Since we have a mutable reference to `Arena`, there are no more references into
        // the boxed bytes. Therefore, it is safe to drop the boxed bytes themselves.
        unsafe {
            for bytes in (*self.allocations.get()).drain(..) {
                drop(Box::from_raw(bytes));
            }
        }
    }

    /// Get the amount of allocations in the arena.
    #[inline]
    pub fn len(&self) -> usize {
        // SAFETY:
        // Accessing `self.allocations` is safe as long as there are no concurrent reads or writes.
        // The _contents_ of `self.allocations` might have outstanding references, but reading the
        // length does not require dereferencing the contents.
        unsafe { (*self.allocations.get()).len() }
    }

    /// Add a set of bytes to the arena, returning a longer-lived mutable reference to a copy of
    /// these same bytes.
    #[inline]
    pub fn add<'a, 'b>(&'a self, bytes: &'b [u8]) -> &'a mut [u8] {
        self.add_boxed(Box::from(bytes))
    }

    /// Add a `Vec<u8>` to the arena, returning a long-lived mutable reference to its contents.
    ///
    /// This method is very similar to `add`, but avoids an allocation and a copy.
    #[inline]
    pub fn add_vec<'a>(&'a self, bytes: Vec<u8>) -> &'a mut [u8] {
        self.add_boxed(bytes.into_boxed_slice())
    }

    /// Add a set of databytes to the arena, returning a longer-lived mutable reference to a copy
    /// of these same databytes.
    #[inline]
    pub fn add_u7<'a, 'b>(&'a self, databytes: &'b [u7]) -> &'a mut [u7] {
        // SAFETY:
        // The returned `&mut [u8]` is transformed into a `&mut [u7]` without checking its
        // contents, which is safe because it was originally a `&[u7]`.
        unsafe { u7::slice_from_int_unchecked_mut(self.add(u7::slice_as_int(databytes))) }
    }

    /// Add a `Vec<u7>` to the arena, returning a long-lived mutable reference to its contents.
    ///
    /// This method is very similar to `add_u7`, but avoids an allocation and a copy.
    #[inline]
    pub fn add_u7_vec<'a>(&'a self, databytes: Vec<u7>) -> &'a mut [u7] {
        // SAFETY:
        // Two unsafe actions are done:
        // First, a `Vec<u7>` is transmuted into a `Vec<u8>`. This is valid because `u7` has the
        // same representation as `u8` (guaranteed by `repr(transparent)`), and every `u7` bit
        // pattern is a valid `u8` bit pattern.
        // Second, the returned `&mut [u8]` is transformed into a `&mut [u7]` without checking its
        // contents, which is safe because it was originally a `Vec<u7>`.
        unsafe {
            u7::slice_from_int_unchecked_mut(
                self.add_vec(mem::transmute::<Vec<u7>, Vec<u8>>(databytes)),
            )
        }
    }

    #[inline]
    fn add_boxed<'a>(&'a self, boxed_bytes: Box<[u8]>) -> &'a mut [u8] {
        // SAFETY:
        // This block moves `boxed_bytes` into `self` and returns a mutable reference to its
        // contents.
        // Further pushes to `self.allocations` may move the _pointers_ to the boxes, but not the
        // box contents themselves, therefore it's safe to hand out mutable references and modify
        // `self.allocations` at the same time.
        unsafe {
            let pointer = Box::into_raw(boxed_bytes);
            (*self.allocations.get()).push(pointer);
            &mut *pointer
        }
    }
}
impl Drop for Arena {
    #[inline]
    fn drop(&mut self) {
        self.clear();
    }
}
// SAFETY: There is no intrinsic issue with moving the arena to another thread, since moving the
// arena guarantees there are no references into it. Any buffers left inside the arena will simply
// be freed from the other thread.
unsafe impl Send for Arena {}
impl fmt::Debug for Arena {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        //TODO: Once the `len()` method for raw pointers to slices is stabilized, add better
        //debug print, showing the size of each allocation.
        write!(f, "Arena({})", self.len())?;
        Ok(())
    }
}
