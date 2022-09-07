use std::alloc::{alloc, Layout};
use std::{fmt, error::*};
use std::cell::UnsafeCell;
use std::ptr;
use std::slice;
use std::str;

fn align_up(addr: usize, alignment: usize) -> usize {
    let remainder = addr % alignment;
    if remainder == 0 {
        return addr;
    } else {
        return addr - remainder + alignment;
    }
}


#[derive(Debug)]
pub enum ArenaError {
    InsufficientSpace,
}

impl Error for ArenaError {}

impl fmt::Display for ArenaError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        write!(f, "Parser Error: {}", self.to_string())
    }
}

pub struct Arena {
    start : UnsafeCell<usize>,
    next : UnsafeCell<usize>,
    end : UnsafeCell<usize>,
}

impl<'arena> Arena {
    pub fn new(capacity : usize) -> Arena {
        
        let next : usize;
        unsafe { 
            let layout = Layout::from_size_align_unchecked(capacity, 1);
            next = alloc(layout) as usize;
        }   

        Arena {
            start : UnsafeCell::new(next),
            next : UnsafeCell::new(next),
            end : UnsafeCell::new(next + capacity)
        }
    }

    pub fn push_str(self : &'arena Arena, input : &str) -> Result<&'arena str, ArenaError> {
        let byte_vec = self.push_slice_copy(input.as_bytes())?;
        unsafe { return Ok(str::from_utf8_unchecked(byte_vec)) };
    }


    pub fn push_slice_copy<T : Copy>(self : &'arena Arena, slice : &[T]) -> Result<&'arena [T], ArenaError>
    {
        let layout = Layout::for_value(slice);
        let dst = self.alloc_layout(layout)?;
        
        unsafe {
            ptr::copy_nonoverlapping(slice.as_ptr(), dst as *mut T, slice.len());
            Ok(slice::from_raw_parts_mut(dst as *mut T, slice.len()))
        }

    }

    pub fn push<T>(self : &'arena Arena, entry : T) -> Result<&'arena T, ArenaError>
    {
        let layout = Layout::for_value(&entry);
        let dst = self.alloc_layout(layout)?;

        unsafe {
            *(dst as *mut T) = entry;
            return Ok(&*(dst as *const T));
        }
    }

    fn alloc_layout(self : &'arena Arena, layout : Layout) -> Result<usize, ArenaError> {
        println!("Size required: {}", layout.size());
        
        // Mutating through an immutable reference is undefined behaviour 
        // - the compiler performs certain optimisations on immutable references.
        // Using UnsafeCell tells the compiler to not perform these optimisations,
        // thus not risking the aforementioned UB.
        let arena_next : *mut usize = self.next.get();
        
        unsafe {
            let dest_start = align_up(*arena_next, layout.align()); 

            // TODO: This could overflow.
            let dest_end = dest_start + layout.size();
            
            let arena_end : *mut usize = self.end.get();

            if dest_end > *arena_end {
                return Err(ArenaError::InsufficientSpace);
            } else {
                *(arena_next) = dest_end;
                return Ok(dest_start);
            }
        } 
    }
}
