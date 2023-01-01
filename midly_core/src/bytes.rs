//! Implementation of the clone-on-write type `Bytes`, specially designed for usage with MIDI
//! messages that carry data.

use crate::prelude::*;
use core::hash;

#[cfg(feature = "alloc")]
use alloc::vec::Vec;

/// An dynamically owned or borrowed slice of bytes, optimized for MIDI message payload.
///
/// Note that this type can only hold up to 4GB of data before panicking.
///
/// # Performance
///
/// This type currently has the same size as a `(usize, u32, u32)` tuple, meaning that on 64-bit
/// systems it is just two words, the same as a `&[u8]` slice.
///
/// Additionally, special care is taken so that addressing the inner `&[u8]` slice performs no
/// branches, allowing for quick access to the underlying bytes.
///
/// Because this type can hold either owned or borrowed data, dropping this type requires a branch
/// to check data ownership.
pub struct Bytes<'a> {
    /// A pointer to either an owned `Vec<u8>` allocation or a `&[u8]` borrowed slice, depending
    /// on the value of `cap`.
    ptr: NonNull<u8>,
    /// The length of the bytes slice. This can be the `Vec::len()` property if the bytes are owned,
    /// or the `slice::len()` property if borrowed.
    ///
    /// In either case, the way of accessing the inner bytes does not change.
    len: u32,
    /// The capacity of the owned `Vec<u8>` (if any).
    ///
    /// If this capacity is zero, the `Bytes` are borrowed. Otherwise, the bytes are owned.
    cap: u32,
    /// Phantom marker. Note that when the lifetime `'a` is `'static`, lifetime restrictions are
    /// lifted.
    _marker: PhantomData<&'a [u8]>,
}
impl Bytes<'static> {
    /// Create a new, empty `Bytes` instance, holding no bytes.
    ///
    /// This is the equivalent of creating a `&[]` slice, and as such creates an instance with
    /// `'static` lifetime.
    #[inline]
    pub fn new() -> Bytes<'static> {
        unsafe { Bytes::from_raw_parts_borrowed(NonNull::dangling(), 0) }
    }

    /// Create an owned `Bytes` instance from an owned `Vec<u8>`.
    ///
    /// This function is only available with the `alloc` feature enabled.
    ///
    /// # Panics
    ///
    /// This function panics if the length or **capacity** of the vector exceeds 4GB.
    #[cfg(feature = "alloc")]
    #[inline]
    pub fn from_vec(mut vec: Vec<u8>) -> Bytes<'static> {
        unsafe {
            let ptr = NonNull::new_unchecked(vec.as_mut_ptr());
            let cap = vec
                .capacity()
                .try_into()
                .expect("vector capacity exceeds 4GB");
            let len = vec.len() as u32;
            mem::forget(vec);
            Self::from_raw_parts_owned(ptr, len, cap)
        }
    }

    /// Create an owned `Bytes` instance from the raw parts of a `Vec<u8>`.
    ///
    /// # Safety
    ///
    /// The raw parts must have been returned by calling `Vec::into_raw_parts` or `Vec::as_ptr`,
    /// `Vec::len` and `Vec::capacity` on a valid `Vec<u8>` instance.
    ///
    /// Note that the capacity must fit in a `u32` (must be less than 4GB), and this implies that
    /// the length also fits in a `u32`.
    #[cfg(feature = "alloc")]
    #[inline]
    pub unsafe fn from_raw_parts_owned(ptr: NonNull<u8>, len: u32, cap: u32) -> Bytes<'static> {
        Bytes {
            ptr,
            len,
            cap,
            _marker: PhantomData,
        }
    }
}
impl<'a> Bytes<'a> {
    /// Create a borrowed `Bytes` instance from a borrowed immutable slice.
    ///
    /// # Panics
    ///
    /// This function panics if the slice is longer than 4GB.
    #[inline]
    pub fn from_slice(slice: &'a [u8]) -> Bytes<'a> {
        unsafe {
            Self::from_raw_parts_borrowed(
                NonNull::new_unchecked(slice.as_ptr() as *mut u8),
                slice.len().try_into().expect("slice length exceeds 4GB"),
            )
        }
    }

    /// Cast the `Bytes<'a>` instance into a `&'a [u8]` instance if the `Bytes` are not owned.
    ///
    /// Note that the lifetime of the returned value differs from the `as_slice` method: the
    /// returned `&'a [u8]` may outlive the `&self` borrow.
    ///
    /// For example, if a `Bytes` instance is created from borrowed bytes and then the
    /// `try_to_slice` method is called on these bytes, the `Bytes` object can be destroyed and the
    /// returned `&'a [u8]` slice will still be valid as long as the *original bytes* are still
    /// borrowed.
    #[inline]
    pub fn try_to_slice(&self) -> Option<&'a [u8]> {
        if self.is_borrowed() {
            // SAFETY: Safe because if the bytes are borrowed, the data outlives `self` and has
            // lifetime `'a`.
            unsafe { Some(mem::transmute::<&[u8], &'a [u8]>(self.as_slice())) }
        } else {
            None
        }
    }

    /// Convert this `Bytes` instance into a vector of bytes, cloning if borrowed and moving if
    /// owned.
    ///
    /// This method is only available with the `alloc` feature enabled.
    #[cfg(feature = "alloc")]
    #[inline]
    pub fn into_vec(mut self) -> Vec<u8> {
        self.take_vec()
    }

    /// Takes the inner bytes as an owned vector, leaving `self` empty.
    /// This method may clone the inner bytes if they are borrowed.
    ///
    /// This method is only available with the `alloc` feature enabled.
    #[cfg(feature = "alloc")]
    #[inline]
    pub fn take_vec(&mut self) -> Vec<u8> {
        unsafe {
            let vec = if self.is_owned() {
                Vec::from_raw_parts(self.ptr.as_ptr(), self.len as usize, self.cap as usize)
            } else {
                self.to_vec()
            };
            ptr::write(self, Bytes::new());
            vec
        }
    }

    /// Tries to take the inner bytes, leaving `self` empty if successful.
    /// This method fails if the data would have to be cloned.
    ///
    /// This method is only available with the `alloc` feature enabled.
    #[cfg(feature = "alloc")]
    #[inline]
    pub fn try_take_vec(&mut self) -> Option<Vec<u8>> {
        if self.is_owned() {
            unsafe {
                let vec =
                    Vec::from_raw_parts(self.ptr.as_ptr(), self.len as usize, self.cap as usize);
                ptr::write(self, Bytes::new());
                Some(vec)
            }
        } else {
            None
        }
    }

    /// Access the inner bytes.
    ///
    /// # Performance
    ///
    /// This method currently performs no computation apart from extending the slice length from
    /// a `u32` to `usize` if necessary.
    /// In particular, no branching is performed.
    #[inline]
    pub fn as_slice(&self) -> &[u8] {
        unsafe { core::slice::from_raw_parts(self.ptr.as_ptr(), self.len as usize) }
    }

    /// Mutates `self` to own its bytes, cloning the bytes if necessary.
    /// Returns a mutable reference to the possibly-cloned bytes.
    ///
    /// This method is only available with the `alloc` feature enabled.
    #[cfg(feature = "alloc")]
    #[inline]
    pub fn make_owned(&mut self) -> &mut [u8] {
        if self.is_borrowed() {
            *self = Bytes::from_vec(self.to_vec());
        }
        unsafe { core::slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len as usize) }
    }

    /// Ensures that the inner bytes are owned, cloning them if necessary.
    /// The returned `Bytes` have a `'static` lifetime, since they no longer reference external
    /// data.
    ///
    /// This method is only available with the `alloc` feature enabled.
    #[cfg(feature = "alloc")]
    #[inline]
    pub fn into_owned(mut self) -> Bytes<'static> {
        unsafe {
            self.make_owned();
            let (ptr, len, cap) = (self.ptr, self.len, self.cap);
            mem::forget(self);
            Bytes::from_raw_parts_owned(ptr, len, cap)
        }
    }

    /// Make the `Bytes` instance writeable (by cloning if necessary) and return a wrapper around
    /// the inner `Vec<u8>`, to modify the inner bytes.
    ///
    /// Note that not dropping the returned `ModifyBytes` instance may not apply any changes made,
    /// and will leave the `Bytes` instance in an unspecified but safe state.
    #[inline]
    pub fn modify<'b: 'a>(&'b mut self) -> ModifyBytes<'b> {
        ModifyBytes::new(self)
    }

    /// Returns `true` if the inner bytes are owned.
    ///
    /// This method is equivalent to `!is_borrowed()`.
    ///
    /// If the `alloc` feature is disabled, this method always returns `false`.
    #[inline]
    pub fn is_owned(&self) -> bool {
        if !cfg!(feature = "alloc") {
            // If allocation is disabled there is no way the capacity is nonzero
            return false;
        }
        self.cap != 0
    }

    /// Returns `true` if the inner bytes are borrowed from an external source.
    ///
    /// This method is equivalent to `!is_owned()`.
    ///
    /// If the `alloc` feature is disabled, this method always returns `true`.
    #[inline]
    pub fn is_borrowed(&self) -> bool {
        if !cfg!(feature = "alloc") {
            // If allocation is disabled there is no way the capacity is nonzero
            return true;
        }
        self.cap == 0
    }

    /// Create a borrowed `Bytes` instance from the raw parts of a `&[u8]` slice.
    ///
    /// # Safety
    ///
    /// The raw parts must correspond to a valid `&[u8]` slice, valid for the `'a` lifetime.
    ///
    /// Note that the length of the slice must fit in a `u32` (must be less than 4GB).
    #[inline]
    pub unsafe fn from_raw_parts_borrowed(ptr: NonNull<u8>, len: u32) -> Bytes<'a> {
        Bytes {
            ptr,
            len,
            cap: 0,
            _marker: PhantomData,
        }
    }
}
// SAFETY: `Bytes` is basically an optimized version of an enum with two variants: one for `Vec<u8>`
// and one for `&[u8]`. Since both of these types are `Send`, `Bytes` should be too.
unsafe impl<'a> Send for Bytes<'a> {}
// SAFETY: The same argument for `Send` runs for `Sync`.
unsafe impl<'a> Sync for Bytes<'a> {}
impl<'a> fmt::Debug for Bytes<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Bytes::")?;
        if self.is_borrowed() {
            write!(f, "Borrowed")?;
        } else {
            write!(f, "Owned")?;
        }
        write!(f, "[")?;
        for &byte in self.iter() {
            write!(f, "{:02x}", byte)?;
        }
        write!(f, "]")?;
        Ok(())
    }
}
impl<'a> Drop for Bytes<'a> {
    #[inline]
    fn drop(&mut self) {
        #[cfg(feature = "alloc")]
        unsafe {
            // Drop the inner vector if the `Bytes` are owned
            if self.is_owned() {
                drop(Vec::from_raw_parts(
                    self.ptr.as_ptr(),
                    self.len as usize,
                    self.cap as usize,
                ));
            }
        }
    }
}
impl<'a> ops::Deref for Bytes<'a> {
    type Target = [u8];
    #[inline]
    fn deref(&self) -> &[u8] {
        self.as_slice()
    }
}
impl<'a> Clone for Bytes<'a> {
    #[inline]
    fn clone(&self) -> Bytes<'a> {
        // Only clone the bytes if they are owned.
        // Otherwise, just re-borrow the source slice.
        match self.try_to_slice() {
            Some(slice) => Bytes::from_slice(slice),
            None => {
                #[cfg(feature = "alloc")]
                {
                    Bytes::from_vec(self.to_vec())
                }
                #[cfg(not(feature = "alloc"))]
                unreachable!()
            }
        }
    }
}
impl<'a> PartialEq for Bytes<'a> {
    #[inline]
    fn eq(&self, rhs: &Self) -> bool {
        PartialEq::<[u8]>::eq(&**self, &**rhs)
    }
}
impl<'a> Eq for Bytes<'a> {}
impl<'a> hash::Hash for Bytes<'a> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, h: &mut H) {
        <[u8] as hash::Hash>::hash(&**self, h)
    }
}
impl<'a> From<&'a [u8]> for Bytes<'a> {
    #[inline]
    fn from(data: &'a [u8]) -> Self {
        Self::from_slice(data)
    }
}
impl<'a> From<&'a mut [u8]> for Bytes<'a> {
    #[inline]
    fn from(data: &'a mut [u8]) -> Self {
        Self::from_slice(data)
    }
}
#[cfg(feature = "alloc")]
impl From<Vec<u8>> for Bytes<'static> {
    #[inline]
    fn from(data: Vec<u8>) -> Self {
        Self::from_vec(data)
    }
}

/// Wrapper around a `Vec<u8>` used when modifying a `Bytes` instance.
/// This type dereferences to `Vec<u8>`.
///
/// This type will apply any changes to the original `Bytes` instance when dropped.
pub struct ModifyBytes<'a> {
    bytes: &'a mut Bytes<'a>,
    vec: Vec<u8>,
}
impl<'a> ModifyBytes<'a> {
    /// Create a new `ModifyBytes` instance that references the given `Bytes` instance.
    pub fn new(bytes: &'a mut Bytes<'a>) -> ModifyBytes<'a> {
        Self {
            vec: bytes.take_vec(),
            bytes,
        }
    }
}
impl<'a> ops::Deref for ModifyBytes<'a> {
    type Target = Vec<u8>;
    fn deref(&self) -> &Vec<u8> {
        &self.vec
    }
}
impl<'a> ops::DerefMut for ModifyBytes<'a> {
    fn deref_mut(&mut self) -> &mut Vec<u8> {
        &mut self.vec
    }
}
impl<'a> Drop for ModifyBytes<'a> {
    fn drop(&mut self) {
        *self.bytes = Bytes::from_vec(mem::replace(&mut self.vec, Vec::new()));
    }
}
