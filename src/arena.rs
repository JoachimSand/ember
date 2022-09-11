use std::alloc::{alloc, Layout};
use std::cell::UnsafeCell;
use std::ptr;
use std::slice;
use std::str;
use std::fmt::Debug;

use crate::compile::CompilationError;

fn align_up(addr: usize, alignment: usize) -> usize {
    let remainder = addr % alignment;
    if remainder == 0 {
        return addr;
    } else {
        return addr - remainder + alignment;
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

    pub fn push_str(self : &'arena Arena, input : &str) -> Result<&'arena str, CompilationError> {
        let byte_vec = self.push_slice_copy(input.as_bytes())?;
        unsafe { return Ok(str::from_utf8_unchecked(byte_vec)) };
    }


    pub fn push_slice_copy<T : Copy + Debug>(self : &'arena Arena, slice : &[T]) -> Result<&'arena [T], CompilationError>
    {
        let layout = Layout::for_value(slice);
        //println!("Allocating slice {:?}, space required: {}", slice, layout.size());
        let dst = self.alloc_layout(layout)? as *mut T;

        unsafe {
            ptr::copy_nonoverlapping(slice.as_ptr(), dst, slice.len());
            Ok(slice::from_raw_parts_mut(dst, slice.len()))
        }

    }

    pub fn push<T>(self : &'arena Arena, entry : T) -> Result<&'arena T, CompilationError>
    {
        let layout = Layout::for_value(&entry);
        let dst = self.alloc_layout(layout)?;

        unsafe {
            *(dst as *mut T) = entry;
            return Ok(&*(dst as *const T));
        }
    }

    fn alloc_layout(self : &'arena Arena, layout : Layout) -> Result<usize, CompilationError> {
        //println!("Size required: {}", layout.size());
        
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
                return Err(CompilationError::InsufficientSpace);
            } else {
                *(arena_next) = dest_end;
                return Ok(dest_start);
            }
        } 
    }
}
